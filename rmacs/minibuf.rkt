#lang racket/base

(provide read-from-minibuffer
         recursive-edit-field-start
         recursive-edit-mode
         recursive-edit-accept-hook
         recursive-edit-cancel-hook)

(require "buffer.rkt")
(require "editor.rkt")
(require "mode.rkt")
(require "keys.rkt")
(require "rope.rkt")

(define (read-from-minibuffer editor
                              prompt
                              #:on-accept k-accept
                              #:on-cancel [k-cancel void])
  (define buf (make-buffer #f "*minibuf*"))
  (configure-fresh-buffer! editor buf)
  (buffer-add-mode! buf recursive-edit-mode)
  (buffer-replace-contents! buf (string->rope prompt))
  (buffer-mark! buf recursive-edit-field-start (buffer-size buf))
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

(define-command recursive-edit-mode (exit-minibuffer buf #:editor ed)
  #:bind-key "C-m"
  #:bind-key "C-j"
  (abandon-recursive-edit ed)
  (select-window ed (recursive-edit-selected-window buf))
  ((recursive-edit-accept-hook buf)
   (rope->string (buffer-region buf recursive-edit-field-start (buffer-size buf)))))
