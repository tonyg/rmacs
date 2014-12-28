#lang racket/base

(provide (except-out (struct-out editor) editor)
         make-editor
         open-window
         close-other-windows
         close-window
         resize-window
         select-window
         visit-file!
         render-editor!
         editor-next-window
         editor-prev-window
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
(require "circular-list.rkt")

(struct editor (buffers ;; BufferGroup
                [tty #:mutable] ;; Tty
                [windows #:mutable] ;; (CircularList (List Window SizeSpec)), abstract window layout
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
                    (list->circular-list (list (list w (relative-size 1))))
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

(define (split-size s)
  (match s
    [(absolute-size _) s] ;; can't scale fixed-size windows
    [(relative-size w) (relative-size (/ w 2))]))

(define (merge-sizes surviving disappearing)
  (match* (surviving disappearing)
    [((relative-size a) (relative-size b)) (relative-size (+ a b))]
    [(_ _) surviving]))

(define (window-for-buffer editor buffer)
  (cond [(circular-list-memf (lambda (e) (eq? (window-buffer (car e)) buffer))
                             (editor-windows editor)) => (compose car circular-car)]
        [else #f]))

(define (entry-for? window) (lambda (e) (eq? (car e) window)))

(define (window->size-spec editor window)
  (cond [(circular-list-memf (entry-for? window)
                             (editor-windows editor)) => (compose cadr circular-car)]
        [else #f]))

(define (update-window-entry editor win updater)
  (set-editor-windows! editor (circular-list-replacef (editor-windows editor)
                                                      (entry-for? win)
                                                      updater)))

(define (open-window editor buffer
                     #:after-window [after-window (editor-active-window editor)]
                     #:proportional? [proportional? #f]
                     #:activate? [activate? #t])
  (define existing-w (window-for-buffer editor buffer))
  (define existing-size (window->size-spec editor after-window))
  (define new-size (if proportional? existing-size (split-size existing-size)))
  (define new-point (or (and existing-w (buffer-mark-pos* buffer (window-point existing-w))) 0))
  (define new-window (make-window buffer new-point))
  (update-window-entry editor after-window
                       (lambda (e) (list (list after-window new-size)
                                         (list new-window new-size))))
  (when activate? (set-editor-active-window! editor new-window))
  new-window)

(define (close-other-windows editor win)
  (for ((entry (circular-list->list (editor-windows editor))) #:when (not (eq? (car entry) win)))
    (set-window-buffer! (car entry) #f))
  (set-editor-windows! editor (list->circular-list (list (list win (relative-size 1)))))
  (set-editor-active-window! editor win))

(define (close-window editor win)
  (define prev (editor-prev-window editor win))
  (define prev-size (window->size-spec editor prev))
  (define win-size (window->size-spec editor win))
  (when (and prev (> (circular-length (editor-windows editor)) 1))
    (when (eq? (editor-active-window editor) win) (set-editor-active-window! editor prev))
    (update-window-entry editor win (lambda (e) '()))
    (resize-window editor prev (merge-sizes prev-size win-size))))

(define (resize-window editor win size)
  (update-window-entry editor win (lambda (e) (list (list win size)))))

(define (select-window editor win)
  (set-editor-active-window! editor win))

(define (visit-file! editor filename)
  (set-window-buffer! (editor-active-window editor)
                      (configure-fresh-buffer! editor
                                               (file->buffer (editor-buffers editor) filename))))

(define (render-editor! editor)
  (render-windows! (editor-tty editor)
                   (circular-list->list (editor-windows editor))
                   (editor-active-window editor)))

(define (editor-active-buffer editor)
  (define w (editor-active-window editor))
  (and w (window-buffer w)))

(define (editor-active-modeset editor)
  (define b (editor-active-buffer editor))
  (and b (buffer-modeset b)))

(define (editor-next-window editor win)
  (cond [(circular-list-memf (entry-for? win)
                             (editor-windows editor)) => (compose car
                                                                  circular-car
                                                                  circular-list-rotate-forward)]
        [else #f]))

(define (editor-prev-window editor win)
  (cond [(circular-list-memf (entry-for? win)
                             (editor-windows editor)) => (compose car
                                                                  circular-car
                                                                  circular-list-rotate-backward)]
        [else #f]))

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
