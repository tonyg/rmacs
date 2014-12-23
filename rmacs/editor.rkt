#lang racket/base

(provide (except-out (struct-out editor) editor)
         make-editor
         visit-file!
         render-editor!
         current-editor-buffer
         current-editor-modeset
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

(struct editor (buffers ;; BufferGroup
                [tty #:mutable] ;; Tty
                [windows #:mutable] ;; (List (List Window SizeSpec)), abstract window layout
                [active-window #:mutable] ;; (Option Window)
                [running? #:mutable] ;; Boolean
                ) #:prefab)

(define (make-editor [tty (stdin-tty)])
  (define g (make-buffergroup))
  (define scratch (make-buffer g "*scratch*" #:initial-contents ";; This is the scratch buffer."))
  (define w (make-window scratch))
  (editor g
          tty
          (list (list w (relative-size 1)))
          w
          #f))

(define (visit-file! editor filename)
  (set-window-buffer! (editor-active-window editor)
                      (file->buffer (editor-buffers editor)
                                    filename)))

(define (render-editor! editor)
  (render-windows! (editor-tty editor)
                   (editor-windows editor)
                   (editor-active-window editor)))

(define (current-editor-buffer editor)
  (define w (editor-active-window editor))
  (and w (window-buffer w)))

(define (current-editor-modeset editor)
  (define b (current-editor-buffer editor))
  (and b (buffer-modeset b)))

(define (root-keyseq-handler editor)
  (modeset-keyseq-handler (current-editor-modeset editor)))

(define (editor-invoke-command selector editor
                               #:keyseq [keyseq #f]
                               #:prefix-arg [prefix-arg '#:default])
  (define cmd (modeset-lookup-command (current-editor-modeset editor) selector))
  (when (not cmd)
    (error 'main "Unhandled command ~a (key sequence: ~a)"
           selector
           (keyseq->keyspec keyseq)))
  (cmd editor prefix-arg keyseq))

(define (editor-mainloop editor)
  (when (editor-running? editor) (error 'editor-mainloop "Nested mainloop"))
  (set-editor-running?! editor #t)
  (with-handlers ([exn? (lambda (e)
                          ;; TODO: proper error reporting
                          (local-require ansi)
                          (tty-restore!)
                          (raise e))])
    (let loop ((keys '())
               (handler (root-keyseq-handler editor)))
      (define (wait-for-input next-handler)
        (render-editor! editor)
        (when (editor-running? editor)
          (sync (handle-evt (tty-next-key-evt (editor-tty editor))
                            (lambda (new-key)
                              (loop (list new-key) next-handler))))))
      (if (null? keys)
          (wait-for-input handler)
          (match (handler editor keys)
            [(unbound-key-sequence)
             (editor-invoke-command 'unbound-key-sequence editor #:keyseq keys)
             (loop '() (root-keyseq-handler editor))]
            [(incomplete-key-sequence next-handler)
             (wait-for-input next-handler)]
            [(key-macro-expansion new-keys)
             (loop new-keys (root-keyseq-handler editor))]
            [(command-invocation selector prefix-arg remaining-input)
             (define accepted-input
               (let loop ((input keys))
                 (if (equal? input remaining-input)
                     '()
                     (cons (car input) (loop (cdr input))))))
             (editor-invoke-command selector editor #:keyseq accepted-input #:prefix-arg prefix-arg)
             (loop remaining-input (root-keyseq-handler editor))])))))

(define (editor-request-shutdown! editor)
  (set-editor-running?! editor #f))

;;---------------------------------------------------------------------------

(define-command kernel-mode (save-buffers-kill-terminal e)
  #:bind-key "C-x C-c"
  (editor-request-shutdown! e))
