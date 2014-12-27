#lang racket/base

(provide (except-out (struct-out editor) editor)
         make-editor
         visit-file!
         render-editor!
         editor-command
         editor-active-buffer
         editor-active-modeset
         editor-mainloop
         editor-request-shutdown!
         editor-force-redisplay!
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
  (initialize-buffergroup! g e)
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

(define (editor-command selector editor
                        #:keyseq [keyseq #f]
                        #:prefix-arg [prefix-arg '#:default])
  (window-command selector (editor-active-window editor) #:keyseq keyseq #:prefix-arg prefix-arg))

(define (root-keyseq-handler editor)
  (modeset-keyseq-handler (editor-active-modeset editor)))

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
               (handler (root-keyseq-handler editor))
               (next-repaint-deadline 0))
      (define (request-repaint) (or next-repaint-deadline (+ (current-inexact-milliseconds) 20)))
      (define (wait-for-input next-handler)
        (when (editor-running? editor)
          (sync (if next-repaint-deadline
                    (handle-evt (alarm-evt next-repaint-deadline)
                                (lambda (_)
                                  (loop total-keyseq '() next-handler next-repaint-deadline)))
                    never-evt)
                (handle-evt (tty-next-key-evt (editor-tty editor))
                            (lambda (new-key)
                              (define new-input (list new-key))
                              (loop (append total-keyseq new-input)
                                    new-input
                                    next-handler
                                    next-repaint-deadline))))))
      (cond
       [(and next-repaint-deadline (>= (current-inexact-milliseconds) next-repaint-deadline))
        (render-editor! editor)
        (loop total-keyseq input handler #f)]
       [(null? input)
        (wait-for-input handler)]
       [else
        (match (handler editor input)
          [(unbound-key-sequence)
           (if (invoke (editor-command 'unbound-key-sequence editor #:keyseq total-keyseq))
               (loop '() '() (root-keyseq-handler editor) (request-repaint))
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
           (invoke (editor-command selector editor #:keyseq accepted-input #:prefix-arg prefix-arg))
           (loop '() remaining-input (root-keyseq-handler editor) (request-repaint))])]))))

(define (editor-request-shutdown! editor)
  (set-editor-running?! editor #f))

(define (editor-force-redisplay! editor)
  (tty-reset (editor-tty editor)))

;;---------------------------------------------------------------------------

(define-command kernel-mode (save-buffers-kill-terminal buf)
  #:bind-key "C-x C-c"
  (editor-request-shutdown! (buffer-editor buf)))

(define-command kernel-mode (force-redisplay buf)
  #:bind-key "C-l"
  (editor-force-redisplay! (buffer-editor buf)))
