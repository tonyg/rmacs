#lang racket/base

(provide (except-out (struct-out editor) editor)
         make-editor
         visit-file!
         render-editor!
         editor-active-buffer
         editor-active-modeset
         editor-invoke-command
         editor-mainloop
         editor-request-shutdown!
         )

(require racket/match)

(require "buffer.rkt")
(require "display.rkt")
(require "window.rkt")
(require "render.rkt")
(require "mode.rkt")
(require "keys.rkt")
(require "rope.rkt")

(struct editor (buffers ;; BufferGroup
                [tty #:mutable] ;; Tty
                [windows #:mutable] ;; (List (List Window SizeSpec)), abstract window layout
                [active-window #:mutable] ;; (Option Window)
                [running? #:mutable] ;; Boolean
                [default-modeset #:mutable] ;; ModeSet
                ) #:prefab)

(define (make-editor #:tty [tty (stdin-tty)]
                     #:default-modeset [default-modeset (make-modeset)])
  (define g (make-buffergroup))
  (define scratch (make-buffer g "*scratch*" #:initial-contents ";; This is the scratch buffer."))
  (define w (make-window scratch))
  (define e (editor g
                    tty
                    (list (list w (relative-size 1)))
                    w
                    #f
                    default-modeset))
  (configure-fresh-buffer! e scratch)
  e)

(define (configure-fresh-buffer! editor buffer)
  (buffer-apply-modeset! buffer (editor-default-modeset editor))
  buffer)

(define (find-buffer editor [title0 #f] #:initial-contents [initial-contents ""])
  (define g (editor-buffers editor))
  (define title (or title0 (unused-buffer-title g '())))
  (or (lookup-buffer g title)
      (configure-fresh-buffer! editor (make-buffer g title #:initial-contents initial-contents))))

(define (open-window editor buffer
                     #:size [size (relative-size 1)]
                     #:activate? [activate? #t])
  (define w (make-window buffer))
  (set-editor-windows! editor (append (editor-windows editor) (list (list w size))))
  (when activate? (set-editor-active-window! editor w))
  w)

(define (visit-file! editor filename)
  (set-window-buffer! (editor-active-window editor)
                      (configure-fresh-buffer! editor
                                               (file->buffer (editor-buffers editor) filename))))

(define (render-editor! editor)
  (render-windows! (editor-tty editor)
                   (editor-windows editor)
                   (editor-active-window editor)))

(define (editor-active-buffer editor)
  (define w (editor-active-window editor))
  (and w (window-buffer w)))

(define (editor-active-modeset editor)
  (define b (editor-active-buffer editor))
  (and b (buffer-modeset b)))

(define (root-keyseq-handler editor)
  (modeset-keyseq-handler (editor-active-modeset editor)))

(define (editor-invoke-command selector editor
                               #:keyseq [keyseq #f]
                               #:prefix-arg [prefix-arg '#:default])
  (define cmd (modeset-lookup-command (editor-active-modeset editor) selector))
  (when (not cmd)
    (error 'editor-invoke-command "Unhandled command ~a (key sequence: ~a)"
           selector
           (keyseq->keyspec keyseq)))
  (cmd editor prefix-arg keyseq))

(define (open-debugger editor exc)
  (local-require (only-in web-server/private/util exn->string))
  (define error-report (exn->string exc))
  (log-error "Exception:\n~a\n" error-report)
  (define b (find-buffer editor "*Error*"))
  (buffer-replace-contents! b (string->rope error-report))
  (open-window editor b))

(define (editor-mainloop editor)
  (when (editor-running? editor) (error 'editor-mainloop "Nested mainloop"))
  (set-editor-running?! editor #t)
  (with-handlers* ([exn? (lambda (exc)
                           (set-editor-running?! editor #f)
                           (open-debugger editor exc)
                           (editor-mainloop editor))])
    (let loop ((total-keyseq '())
               (input '())
               (handler (root-keyseq-handler editor)))
      (define (wait-for-input next-handler)
        (render-editor! editor)
        (when (editor-running? editor)
          (sync (handle-evt (tty-next-key-evt (editor-tty editor))
                            (lambda (new-key)
                              (define new-input (list new-key))
                              (loop (append total-keyseq new-input) new-input next-handler))))))
      (if (null? input)
          (wait-for-input handler)
          (match (handler editor input)
            [(unbound-key-sequence)
             (if (editor-invoke-command 'unbound-key-sequence editor #:keyseq total-keyseq)
                 (loop '() '() (root-keyseq-handler editor))
                 (error 'editor-mainloop "Unbound key sequence: ~a"
                        (keyseq->keyspec total-keyseq)))]
            [(incomplete-key-sequence next-handler)
             (wait-for-input next-handler)]
            [(command-invocation selector prefix-arg remaining-input)
             (define accepted-input
               (let remove-tail ((keyseq total-keyseq))
                 (if (equal? keyseq remaining-input)
                     '()
                     (cons (car keyseq) (remove-tail (cdr keyseq))))))
             (editor-invoke-command selector editor #:keyseq accepted-input #:prefix-arg prefix-arg)
             (loop '() remaining-input (root-keyseq-handler editor))])))))

(define (editor-request-shutdown! editor)
  (set-editor-running?! editor #f))

;;---------------------------------------------------------------------------

(define-command kernel-mode (save-buffers-kill-terminal e)
  #:bind-key "C-x C-c"
  (editor-request-shutdown! e))
