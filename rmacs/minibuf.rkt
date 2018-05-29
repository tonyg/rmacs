#lang racket/base

(provide minibuffer-history
         read-from-minibuffer
         string-arg
         read-string-from-minibuffer
         recursive-edit-field-start
         recursive-edit-mode
         recursive-edit-accept-hook
         recursive-edit-cancel-hook
         recursive-edit-acceptable-hook
         completing-read
         simple-completion
         completing-read-mode
         completing-read-string=?-hook
         completing-read-completion-hook)

(require "buffer.rkt")
(require "editor.rkt")
(require "mode.rkt")
(require "keys.rkt")
(require "rope.rkt")
(require "mark.rkt")
(require "window.rkt")
(require "strings.rkt")
(require "history.rkt")

;;---------------------------------------------------------------------------

(define-editor-local minibuffer-history (make-history))

(define (read-from-minibuffer editor
                              prompt
                              #:history [history (minibuffer-history editor)]
                              #:initial [initial ""]
                              #:defaults [defaults '()]
                              #:acceptable? [acceptable? (lambda (v) #t)]
                              #:on-accept k-accept
                              #:on-cancel [k-cancel void])
  (define buf (make-buffer #f "*minibuf*"))
  (configure-fresh-buffer! editor buf)
  (buffer-add-mode! buf recursive-edit-mode)
  (buffer-replace-contents! buf (piece->rope prompt))
  (buffer-mark! buf recursive-edit-field-start (buffer-size buf))
  (set-recursive-edit-contents! buf initial)
  (recursive-edit-selected-window buf (editor-active-window editor))
  (recursive-edit-history buf (or history (make-history))) ;; #f -> transient history
  (recursive-edit-defaults buf defaults)
  (recursive-edit-history-index buf 0)
  (recursive-edit-new-input buf "")
  (recursive-edit-accept-hook buf k-accept)
  (recursive-edit-cancel-hook buf k-cancel)
  (recursive-edit-acceptable-hook buf acceptable?)
  (start-recursive-edit editor buf)
  buf)

(define ((string-arg prompt
                     #:history [history-fn minibuffer-history]
                     #:initial [initial ""]
                     #:defaults [defaults-fn (lambda (ed) '())]
                     #:acceptable? [acceptable? (lambda (v) #t)])
         ed sig argname k)
  (read-string-from-minibuffer ed prompt
                               #:history (history-fn ed)
                               #:initial initial
                               #:defaults (defaults-fn ed)
                               #:acceptable? acceptable?
                               #:on-accept k))

(define (read-string-from-minibuffer editor
                                     prompt
                                     #:history [history (minibuffer-history editor)]
                                     #:initial [initial ""]
                                     #:defaults [defaults '()]
                                     #:acceptable? [acceptable? (lambda (v) #t)]
                                     #:on-accept k-accept
                                     #:on-cancel [k-cancel void])
  (read-from-minibuffer editor
                        prompt
                        #:history history
                        #:initial initial
                        #:defaults defaults
                        #:acceptable? acceptable?
                        #:on-accept (lambda (result)
                                      (if (and (pair? defaults) (string=? result ""))
                                          (k-accept (car defaults))
                                          (k-accept result)))
                        #:on-cancel k-cancel))

(define recursive-edit-field-start (mark-type (buffer-mark-type 'recursive-edit-field-start
                                                                '*minibuf*
                                                                #t)
                                              'left))

(define recursive-edit-mode
  (mode-add-constraints (make-mode "recursive-edit")
                        #:dispatch-keys-before '(#:minibuf)
                        #:interpret-commands-before '(#:minibuf)))

(define-buffer-local recursive-edit-selected-window)
(define-buffer-local recursive-edit-history)
(define-buffer-local recursive-edit-defaults '())
(define-buffer-local recursive-edit-history-index 0)
(define-buffer-local recursive-edit-new-input "")
(define-buffer-local recursive-edit-accept-hook (lambda (content) (void)))
(define-buffer-local recursive-edit-cancel-hook (lambda () (void)))
(define-buffer-local recursive-edit-acceptable-hook (lambda (v) #t))

(define-simple-command-signature (abort-recursive-edit))
(define-simple-command-signature (exit-minibuffer))
(define-simple-command-signature (minibuf-beginning-of-line))
(define-simple-command-signature (next-history-element))
(define-simple-command-signature (previous-history-element))

(define-command recursive-edit-mode cmd:abort-recursive-edit (#:buffer buf #:editor ed)
  #:bind-key "C-g"
  (abandon-recursive-edit ed)
  (select-window ed (recursive-edit-selected-window buf))
  ((recursive-edit-cancel-hook buf)))

(define (recursive-edit-contents buf)
  (rope->searchable-string (buffer-region buf recursive-edit-field-start (buffer-size buf))))

(define (set-recursive-edit-contents! buf str #:notify? [notify? #t])
  (buffer-region-update! buf recursive-edit-field-start (buffer-size buf)
                         (lambda (_old) (piece->rope str))
                         #:notify? notify?))

(define-command recursive-edit-mode cmd:exit-minibuffer (#:buffer buf #:editor ed)
  #:bind-key "<return>"
  #:bind-key "C-j"
  (define result (recursive-edit-contents buf))
  (when ((recursive-edit-acceptable-hook buf) result)
    (abandon-recursive-edit ed)
    (select-window ed (recursive-edit-selected-window buf))
    (define maybe-revised-result ((recursive-edit-accept-hook buf) result))
    (when (string? maybe-revised-result)
      (history-push! (recursive-edit-history buf) maybe-revised-result))))

(define-command recursive-edit-mode cmd:minibuf-beginning-of-line (#:buffer buf #:window win)
  #:bind-key "C-a"
  #:bind-key "<home>"
  (define limit (buffer-mark-pos* buf recursive-edit-field-start))
  (if (and limit (> (buffer-mark-pos buf (window-point win)) limit))
      (window-move-to! win limit)
      (buffer-move-mark-to-start-of-line! buf (window-point win))))

(define-command recursive-edit-mode cmd:next-history-element
  (#:buffer buf #:window win #:editor ed)
  #:bind-key "M-n"
  (adjust-history-index! ed win buf -1))

(define-command recursive-edit-mode cmd:previous-history-element
  (#:buffer buf #:window win #:editor ed)
  #:bind-key "M-p"
  (adjust-history-index! ed win buf 1))

(define (adjust-history-index! ed win buf delta)
  (define defaults (recursive-edit-defaults buf))
  (define h (recursive-edit-history buf))
  (define old-pos (recursive-edit-history-index buf))
  (define new-pos (+ old-pos delta))
  (define lo-limit (- (length defaults)))
  (define hi-limit (history-length h))
  (when (< new-pos lo-limit)
    (if (zero? lo-limit)
        (abort "End of history; no default available")
        (abort "End of defaults; no next item")))
  (when (> new-pos hi-limit)
    (abort "Beginning of history; no preceding item"))
  (when (zero? old-pos)
    (recursive-edit-new-input buf (recursive-edit-contents buf)))
  (recursive-edit-history-index buf new-pos)
  (set-recursive-edit-contents!
   buf
   (cond [(positive? new-pos) (history-ref h (- new-pos 1))]
         [(zero? new-pos) (recursive-edit-new-input buf)]
         [(negative? new-pos) (list-ref defaults (- (- new-pos) 1))])))

;;---------------------------------------------------------------------------

(define (completing-read editor
                         prompt
                         completion-fn
                         #:string=? [string=? string=?]
                         #:history [history (minibuffer-history editor)]
                         #:initial [initial ""]
                         #:defaults [defaults '()]
                         #:acceptable? [acceptable? (lambda (v) #t)]
                         #:on-accept k-accept
                         #:on-cancel [k-cancel void])
  (define buf (read-from-minibuffer editor prompt
                                    #:history history
                                    #:initial initial
                                    #:defaults defaults
                                    #:acceptable? acceptable?
                                    #:on-accept k-accept
                                    #:on-cancel k-cancel))
  (buffer-add-mode! buf completing-read-mode)
  (completing-read-string=?-hook buf string=?)
  (completing-read-completion-hook buf completion-fn)
  buf)

(define (simple-completion collection)
  (define collection-strings (for/list ((c collection)) (format "~a" c)))
  (lambda (prefix string=?)
    (for/list ((c collection-strings) #:when (string-prefix? prefix c string=?)) c)))

(define completing-read-mode
  (mode-add-constraints (make-mode "completing")
                        #:dispatch-keys-before (list '#:minibuf recursive-edit-mode)
                        #:interpret-commands-before (list '#:minibuf recursive-edit-mode)))

(define-buffer-local completing-read-string=?-hook
  string=?)
(define-buffer-local completing-read-completion-hook
  (lambda (v) (abort "completing-read-completion-hook not set")))

(define (common-string-prefix strs string=?)
  (if (null? (cdr strs))
      (car strs)
      (let ((len (let loop ((i 1))
                   (if (and (>= (string-length (car strs)) i)
                            (for/and ((c (cdr strs)))
                              (and (>= (string-length c) i)
                                   (string=? (substring (car strs) 0 i) (substring c 0 i)))))
                       (loop (+ i 1))
                       (- i 1)))))
        (substring (car strs) 0 len))))

(define-simple-command-signature (minibuffer-complete))

(define-command completing-read-mode cmd:minibuffer-complete (#:buffer buf #:editor ed)
  #:bind-key "<tab>"
  (define string=? (completing-read-string=?-hook buf))
  (define prefix (recursive-edit-contents buf))
  (define completions ((completing-read-completion-hook buf) prefix string=?))
  (if (pair? completions)
      (let ((common-prefix (common-string-prefix completions string=?))
            (complete? (null? (cdr completions))))
        (if (string=? common-prefix prefix)
            ;; No progress.
            (if complete?
                (message ed "Sole completion")
                (message ed "Completions: ~a" completions))
            ;; Some progress
            (buffer-region-update! buf
                                   recursive-edit-field-start
                                   (buffer-size buf)
                                   (lambda (_old)
                                     (piece->rope common-prefix)))))
      (message ed "No match")))
