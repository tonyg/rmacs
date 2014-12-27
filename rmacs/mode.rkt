#lang racket/base
;; Modes and modesets.

(provide (struct-out mode)
         (struct-out modeset)
         (struct-out incomplete-key-sequence)
         (struct-out unbound-key-sequence)
         (struct-out command-invocation)

         make-raw-mode
         make-mode
         mode-add-constraints
         mode-keymap-bind!
         mode-keymap-unbind!
         mode-keymap-rebind!
         mode-define-command!
         mode-undefine-command!
         mode-redefine-command!

         make-modeset
         modeset-add-mode
         modeset-remove-mode
         modeset-toggle-mode
         modeset-keyseq-handler
         modeset-lookup-command

         kernel-mode
         kernel-modeset)

(require racket/set)
(require racket/match)
(require (only-in racket/list filter-map))

(require "keys.rkt")
(require "topsort.rkt")

(struct mode (id
              name
              [keymap #:mutable]
              [commands #:mutable]
              dispatch-keys-before
              dispatch-keys-after
              interpret-commands-before
              interpret-commands-after
              ) #:prefab)

(struct modeset (modes
                 key-dispatch-order
                 command-interpretation-order
                 ) #:prefab)

(struct incomplete-key-sequence (handler) #:prefab)
(struct unbound-key-sequence () #:prefab)
(struct command-invocation (selector prefix-arg remaining-input) #:prefab)

(define (make-raw-mode name)
  (mode (gensym name)
        name
        (empty-keymap)
        (hasheq)
        (seteq)
        (seteq)
        (seteq)
        (seteq)))

(define (mode-add-constraints m
                              #:dispatch-keys-before [kb '()]
                              #:dispatch-keys-after [ka '()]
                              #:interpret-commands-before [cb '()]
                              #:interpret-commands-after [ca '()])
  (define (convert modes) (list->seteq (for/list ((m modes))
                                         (if (keyword? m)
                                             m
                                             (mode-id m)))))
  (struct-copy mode m
               [dispatch-keys-before
                (set-union (mode-dispatch-keys-before m) (convert kb))]
               [dispatch-keys-after
                (set-union (mode-dispatch-keys-after m) (convert ka))]
               [interpret-commands-before
                (set-union (mode-interpret-commands-before m) (convert cb))]
               [interpret-commands-after
                (set-union (mode-interpret-commands-after m) (convert ca))]))

(define (make-mode name)
  (mode-add-constraints (make-raw-mode name)
                        #:dispatch-keys-before '(#:kernel)
                        #:interpret-commands-before '(#:kernel)))

(define (mode-keymap-bind! m keyspec command)
  (set-mode-keymap! m (keymap-bind (mode-keymap m) keyspec command))
  m)

(define (mode-keymap-unbind! m keyspec)
  (set-mode-keymap! m (keymap-unbind (mode-keymap m) keyspec))
  m)

(define (mode-keymap-rebind! m keyspec command)
  (mode-keymap-bind! (mode-keymap-unbind! m keyspec) keyspec command))

(define (mode-define-command! m selector handler)
  (when (hash-has-key? (mode-commands m) selector)
    (error 'mode-define-command!
           "Duplicate command handler for ~a in mode ~a"
           selector
           (mode-id m)))
  (set-mode-commands! m (hash-set (mode-commands m) selector handler))
  m)

(define (mode-undefine-command! m selector)
  (set-mode-commands! m (hash-remove (mode-commands m) selector))
  m)

(define (mode-redefine-command! m selector handler)
  (mode-define-command! (mode-undefine-command! m selector) selector handler))

(define (make-modeset)
  (modeset (hasheq)
           '()
           '()))

(define (modeset-add-mode ms m)
  (compute-modeset-orders
   (struct-copy modeset ms [modes (hash-set (modeset-modes ms)
                                            (mode-id m)
                                            m)])))

(define (modeset-remove-mode ms m)
  (compute-modeset-orders
   (struct-copy modeset ms [modes (hash-remove (modeset-modes ms) (mode-id m))])))

(define (modeset-toggle-mode ms m)
  ((if (hash-has-key? (modeset-modes ms) (mode-id m)) modeset-remove-mode modeset-add-mode)
   ms
   m))

(define (edges ms before-getter after-getter)
  (for/fold [(es '())]
            [(m (in-hash-values (modeset-modes ms)))]
    (define mid (mode-id m))
    (append (for/list [(nid (before-getter m))] (list mid nid))
            (for/list [(nid (after-getter m))] (list nid mid))
            es)))

(define (compute-modeset-order ms what before-getter after-getter)
  (or (topsort (edges ms before-getter after-getter) #:comparison eq?)
      (error 'compute-modeset-orders "Inconsistent ~a order: ~v"
             (hash-keys (modeset-modes ms)))))

(define (compute-modeset-orders ms)
  (struct-copy modeset ms
   [key-dispatch-order (compute-modeset-order ms
                                              "key dispatch"
                                              mode-dispatch-keys-before
                                              mode-dispatch-keys-after)]
   [command-interpretation-order (compute-modeset-order ms
                                                        "command interpretation"
                                                        mode-interpret-commands-before
                                                        mode-interpret-commands-after)]))

(define (order->modes ms order-getter)
  (define modes (modeset-modes ms))
  (filter-map (lambda (id) (hash-ref modes id #f)) (order-getter ms)))

(define (modeset-keyseq-handler ms)
  (let handler-for-maps ((maps (map mode-keymap (order->modes ms modeset-key-dispatch-order))))
    (lambda (e ks)
      (define results (map (lambda (km)
                             (define-values (result remaining-input) (keymap-lookup km ks))
                             (list result remaining-input)) maps))
      (let process-results ((results results))
        (match results
          ['() (unbound-key-sequence)]
          [(cons (list result remaining-input) rest)
           (cond
            [(not result) (process-results rest)]
            [(keymap? result) (incomplete-key-sequence
                               (handler-for-maps (filter keymap? (map car results))))]
            [(procedure? result)
             (if (null? remaining-input)
                 (incomplete-key-sequence result)
                 (result e remaining-input))]
            [else (command-invocation result '#:default remaining-input)])])))))

(define (modeset-lookup-command ms selector)
  (let search ((tables (map mode-commands
                            (order->modes ms modeset-command-interpretation-order))))
    (match tables
      ['() #f]
      [(cons table rest)
       (define handler (hash-ref table selector #f))
       (if handler
           (lambda (cmd)
             (handler cmd
                      (lambda ([cmd cmd])
                        (define next-method (search rest))
                        (when next-method (next-method cmd)))))
           (search rest))])))

(define kernel-mode
  (mode-add-constraints (make-raw-mode "kernel")
                        #:dispatch-keys-after '(#:kernel)
                        #:interpret-commands-after '(#:kernel)))

(define kernel-modeset
  (modeset-add-mode (make-modeset) kernel-mode))
