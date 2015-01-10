#lang racket/base
;; Modes and modesets.

(provide (struct-out mode)
         (struct-out modeset)
         (struct-out incomplete-key-sequence)
         (struct-out unbound-key-sequence)
         (struct-out command-invocation)
         (struct-out command-signature)
         (struct-out command-argument-spec)

         command-signature->list

         make-raw-mode
         make-mode
         mode-add-constraints
         mode-keymap-bind!
         mode-keymap-unbind!
         mode-keymap-rebind!
         mode-define-signature!
         mode-undefine-signature!
         mode-lookup-signature
         mode-define-command!
         mode-undefine-command!
         mode-redefine-command!
         mode-command-signatures

         make-modeset
         modeset-add-mode
         modeset-remove-mode
         modeset-toggle-mode
         modeset-keyseq-handler
         modeset-lookup-command
         modeset-lookup-signature
         modeset-command-signatures

         define-command-signature
         define-simple-command-signature
         collect-args
         const-arg

         kernel-mode
         kernel-modeset)

(require (for-syntax syntax/parse))
(require (for-syntax racket/base))
(require (for-syntax "syntax.rkt"))

(require racket/set)
(require racket/match)
(require (only-in racket/list filter-map))

(require "keys.rkt")
(require "topsort.rkt")

(struct mode (id
              name
              [keymap #:mutable]
              [signatures #:mutable] ;; (Hasheq Symbol Signature)
              [commands #:mutable] ;; (Hasheq Signature Handler)
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
(struct command-invocation (signature prefix-arg remaining-input) #:prefab)

;; A CommandCategory is one of
;; -- 'interactive
;; -- 'event

(struct command-signature (selector ;; Symbol
                           category ;; CommandCategory
                           args ;; (List of Argspec)
                           ) #:prefab)

(struct command-argument-spec (name ;; Symbol
                               value-proc
                               ;; ^ CPS value-producing function:
                               ;; (Editor Signature Symbol (-> Any1 Any2) -> Any3)
                               ) #:prefab)

(define (command-signature->list sig)
  (cons (command-signature-selector sig)
        (map command-argument-spec-name (command-signature-args sig))))

(define (make-raw-mode name)
  (mode (gensym name)
        name
        (empty-keymap)
        (hasheq)
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

(define (mode-define-signature! m signature)
  (define selector (command-signature-selector signature))
  (let ((existing-sig (mode-lookup-signature m selector)))
    (when (and existing-sig (not (eq? existing-sig signature)))
      (error 'mode-define-signature!
             "Cannot overwrite existing signature ~a with new signature ~a in mode ~a"
             (command-signature->list existing-sig)
             (command-signature->list signature)
             (mode-id m))))
  (set-mode-signatures! m (hash-set (mode-signatures m) selector signature))
  m)

(define (mode-undefine-signature! m signature)
  (define selector (command-signature-selector signature))
  (define existing-sig (mode-lookup-signature m selector))
  (when (and existing-sig (not (eq? existing-sig signature)))
    (error 'mode-undefine-signature!
           "Attempt to remove signature ~a that conflicts with existing signature ~a in mode ~a"
           (command-signature->list signature)
           (command-signature->list existing-sig)
           (mode-id m)))
  (set-mode-signatures! m (hash-remove (mode-signatures m) selector))
  m)

(define (mode-lookup-signature m selector)
  (hash-ref (mode-signatures m) selector #f))

(define (mode-define-command! m signature handler)
  (mode-define-signature! m signature)
  (when (hash-has-key? (mode-commands m) signature)
    (error 'mode-define-command!
           "Duplicate handler for command ~a in mode ~a"
           (command-signature->list signature)
           (mode-id m)))
  (set-mode-commands! m (hash-set (mode-commands m) signature handler))
  m)

(define (mode-undefine-command! m signature)
  (set-mode-commands! m (hash-remove (mode-commands m) signature))
  m)

(define (mode-redefine-command! m signature handler)
  (mode-define-command! (mode-undefine-command! m signature) signature handler))

(define (mode-command-signatures m)
  (list->seteq (hash-keys (mode-commands m))))

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
            [(command-signature? result)
             (command-invocation result '#:default remaining-input)]
            [else (error 'modeset-keyseq-handler "Invalid keymap-lookup result: ~v" result)])])))))

(define (modeset-lookup-command ms signature)
  (let search ((tables (map mode-commands
                            (order->modes ms modeset-command-interpretation-order))))
    (match tables
      ['() #f]
      [(cons table rest)
       (match (hash-ref table signature #f)
         [#f (search rest)]
         [handler (lambda (cmd)
                    (handler cmd
                             (lambda ([cmd cmd])
                               (define next-method (search rest))
                               (when next-method (next-method cmd)))))])])))

(define (modeset-lookup-signature ms selector)
  (for/or ((m (order->modes ms modeset-command-interpretation-order)))
    (mode-lookup-signature m selector)))

(define (modeset-command-signatures ms)
  (for/fold [(signatures (seteq))] [(m (hash-values (modeset-modes ms)))]
    (set-union signatures (mode-command-signatures m))))

(define-syntax define-command-signature
  (lambda (stx)
    (syntax-parse stx
      [(_ id (selector [argname argproc] ...)
          (~optional (~seq #:category category) #:defaults ([category #'interactive])))
       #'(define id (command-signature 'selector
                                       'category
                                       (list (command-argument-spec 'argname argproc) ...)))])))

(define-syntax define-simple-command-signature
  (lambda (stx)
    (syntax-parse stx
      [(_ (selector arg ...) other ...)
       #`(define-command-signature #,(build-name #'selector "cmd:" #'selector)
           (selector arg ...)
           other ...)])))

(define (collect-args sig editor k)
  (let loop ((specs (command-signature-args sig)) (acc '()))
    (match specs
      ['()
       (k (reverse acc))]
      [(cons (command-argument-spec name value-proc) rest)
       (value-proc editor sig name (lambda (v) (loop rest (cons v acc))))])))

(define (const-arg v)
  (lambda (ed sig name k) (k v)))

(define kernel-mode
  (mode-add-constraints (make-raw-mode "kernel")
                        #:dispatch-keys-after '(#:kernel)
                        #:interpret-commands-after '(#:kernel)))

(define kernel-modeset
  (modeset-add-mode (make-modeset) kernel-mode))
