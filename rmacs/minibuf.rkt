#lang racket/base

(provide read-from-minibuffer
         recursive-edit-field-start
         recursive-edit-mode
         recursive-edit-accept-hook
         recursive-edit-cancel-hook
         completing-read
         simple-completion
         completing-read-mode
         completing-read-string=?-hook
         completing-read-completion-hook
         completing-read-acceptable-hook)

(require "buffer.rkt")
(require "editor.rkt")
(require "mode.rkt")
(require "keys.rkt")
(require "rope.rkt")
(require "window.rkt")
(require "strings.rkt")

;;---------------------------------------------------------------------------

(define (read-from-minibuffer editor
                              prompt
                              #:initial [initial ""]
                              #:on-accept k-accept
                              #:on-cancel [k-cancel void])
  (define buf (make-buffer #f "*minibuf*"))
  (configure-fresh-buffer! editor buf)
  (buffer-add-mode! buf recursive-edit-mode)
  (buffer-replace-contents! buf (string->rope prompt))
  (buffer-mark! buf recursive-edit-field-start (buffer-size buf))
  (buffer-insert! buf (buffer-size buf) (string->rope initial))
  (recursive-edit-selected-window buf (editor-active-window editor))
  (recursive-edit-accept-hook buf k-accept)
  (recursive-edit-cancel-hook buf k-cancel)
  (start-recursive-edit editor buf)
  buf)

(define recursive-edit-field-start (mark-type (buffer-mark-type 'recursive-edit-field-start
                                                                '*minibuf*
                                                                #t)
                                              'left))

(define recursive-edit-mode (make-mode "recursive-edit"))

(define-buffer-local recursive-edit-selected-window)
(define-buffer-local recursive-edit-accept-hook (lambda (content) (void)))
(define-buffer-local recursive-edit-cancel-hook (lambda () (void)))

(define-command recursive-edit-mode (abort-recursive-edit buf #:editor ed)
  #:bind-key "C-g"
  (abandon-recursive-edit ed)
  (select-window ed (recursive-edit-selected-window buf))
  ((recursive-edit-cancel-hook buf)))

(define (recursive-edit-contents buf)
  (rope->string (buffer-region buf recursive-edit-field-start (buffer-size buf))))

(define-command recursive-edit-mode (exit-minibuffer buf #:editor ed)
  #:bind-key "C-m"
  #:bind-key "C-j"
  (abandon-recursive-edit ed)
  (select-window ed (recursive-edit-selected-window buf))
  ((recursive-edit-accept-hook buf) (recursive-edit-contents buf)))

(define-command recursive-edit-mode (minibuf-beginning-of-line buf #:window win)
  #:bind-key "C-a"
  #:bind-key "<home>"
  (define limit (buffer-mark-pos* buf recursive-edit-field-start))
  (if (and limit (> (buffer-mark-pos buf (window-point win)) limit))
      (window-move-to! win limit)
      (buffer-move-mark-to-start-of-line! buf (window-point win))))

;;---------------------------------------------------------------------------

(define (completing-read editor
                         prompt
                         completion-fn
                         #:string=? [string=? string=?]
                         #:initial [initial ""]
                         #:acceptable? [acceptable? (lambda (v) #t)]
                         #:on-accept k-accept
                         #:on-cancel [k-cancel void])
  (define buf (read-from-minibuffer editor prompt
                                    #:initial initial
                                    #:on-accept k-accept
                                    #:on-cancel k-cancel))
  (buffer-add-mode! buf completing-read-mode)
  (completing-read-string=?-hook buf string=?)
  (completing-read-completion-hook buf completion-fn)
  (completing-read-acceptable-hook buf acceptable?)
  buf)

(define (simple-completion collection)
  (define collection-strings (for/list ((c collection)) (format "~a" c)))
  (lambda (prefix string=?)
    (for/list ((c collection-strings) #:when (string-prefix? prefix c string=?)) c)))

(define completing-read-mode (make-mode "completing"))

(define-buffer-local completing-read-string=?-hook
  string=?)
(define-buffer-local completing-read-completion-hook
  (lambda (v) (abort "completing-read-completion-hook not set")))
(define-buffer-local completing-read-acceptable-hook
  (lambda (v) #t))

(define (common-string-prefix strs string=?)
  (if (null? (cdr strs))
      (car strs)
      (let ((len (let loop ((i 1))
                   (if (for/and ((c (cdr strs)))
                         (and (>= (string-length c) i)
                              (string=? (substring (car strs) 0 i) (substring c 0 i))))
                       (loop (+ i 1))
                       (- i 1)))))
        (substring (car strs) 0 len))))

(define-command completing-read-mode (minibuffer-complete buf #:editor ed)
  #:bind-key "C-i"
  #:bind-key "tab"
  (define string=? (completing-read-string=?-hook buf))
  (define prefix (recursive-edit-contents buf))
  (define unfiltered-completions ((completing-read-completion-hook buf) prefix string=?))
  (define completions (filter (lambda (s) (string-prefix? prefix s string=?))
                              unfiltered-completions))
  (when (pair? completions)
    (define common-prefix (common-string-prefix completions string=?))
    (define complete? (null? (cdr completions)))
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
                                 (string->rope common-prefix))))))

(define-command completing-read-mode (exit-minibuffer buf
                                                      #:next-method next-method
                                                      #:command cmd
                                                      #:editor ed)
  (when ((completing-read-acceptable-hook buf) (recursive-edit-contents buf))
    (next-method cmd)))
