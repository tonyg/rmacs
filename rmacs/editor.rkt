#lang racket/base

(provide (except-out (struct-out editor) editor)
         make-editor
         configure-fresh-buffer!
         find-buffer
         window-layout
         open-window
         close-other-windows
         close-window
         resize-window
         select-window
         windows-for-buffer
         window-for-buffer
         visit-file!
         render-editor!
         editor-next-window
         editor-prev-window
         editor-command
         invoke/history
         collect-args-and-invoke/history
         editor-last-command?
         editor-active-buffer
         editor-active-modeset
         cmd:unbound-key-sequence
         editor-mainloop
         editor-request-shutdown!
         editor-force-redisplay!
         editor-sit-for
         set-editor-event!
         set-editor-timeout!
         clear-editor-event!
         clear-message
         message
         start-recursive-edit
         abandon-recursive-edit
         define-editor-local)

(require racket/match)

(require "buffer.rkt")
(require "display.rkt")
(require "display-terminal.rkt")
(require "display-gui.rkt")
(require "window.rkt")
(require "render.rkt")
(require "mode.rkt")
(require "keys.rkt")
(require "rope.rkt")
(require "circular-list.rkt")
(require "file.rkt")
(require "local.rkt")
(require "colorize.rkt")

(struct editor (buffers ;; BufferGroup
                [tty #:mutable] ;; Tty
                [windows #:mutable] ;; (CircularList (List Window SizeSpec)), abstract window layout
                [active-window #:mutable] ;; (Option Window)
                [running? #:mutable] ;; Boolean
                [default-modeset #:mutable] ;; ModeSet
                [layout #:mutable] ;; (Option (List Layout))
                [last-command #:mutable] ;; (Option Command)
                echo-area ;; Buffer
                mini-window ;; Window
                [active-events #:mutable] ;; (Hash Any Evt)
                [recursive-edit #:mutable] ;; (Option Buffer)
                [locals #:mutable] ;; LocalsTable
                ) #:prefab)

(define (make-editor #:tty [tty (default-tty)]
                     #:default-modeset [default-modeset (make-modeset)])
  (define g (make-buffergroup))
  (define scratch (make-buffer g "*scratch*"
                               #:initial-contents ";; This is the scratch buffer.\n\n"))
  (define echo-area (make-buffer #f "*echo-area*"))
  (define w (make-window scratch))
  (define ws (list->circular-list (list (list w (relative-size 1)))))
  (define miniwin (make-window echo-area))
  (define e (editor g ;; buffers
                    tty ;; tty
                    ws ;; windows
                    w ;; active-window
                    #f ;; running?
                    default-modeset ;; default-modeset
                    #f ;; layout
                    #f ;; last-command
                    echo-area ;; echo-area
                    miniwin ;; mini-window
                    (hash) ;; active-events
                    #f ;; recursive-edit
                    (make-locals) ;; locals
                    ))
  (initialize-buffergroup! g e)
  (configure-fresh-buffer! e scratch)
  (window-move-to! w (buffer-size scratch))
  (set-window-status-line?! miniwin #f)
  e)

(define (colorize! editor buf)
  (local-require "timing.rkt")
  (when (time* (list 'colorizing (buffer-title buf)) (colorize-burst buf))
    (set-editor-event! editor
                       colorize!
                       (handle-evt always-evt
                                   (lambda (_)
                                     (clear-editor-event! editor colorize!)
                                     (colorize! editor buf)
                                     #t)))))

(define (configure-fresh-buffer! editor buffer)
  (buffer-apply-modeset! buffer (editor-default-modeset editor))
  (colorize! editor buffer)
  buffer)

(define (find-buffer editor [title0 #f] #:initial-contents [initial-contents ""])
  (define g (editor-buffers editor))
  (define title (or title0 (unused-buffer-title g '())))
  (or (lookup-buffer g title)
      (configure-fresh-buffer! editor (make-buffer g title #:initial-contents initial-contents))))

(define (split-size s)
  (match s
    [(absolute-size _) (relative-size 1)] ;; can't scale fixed-size windows
    [(relative-size w) (relative-size (/ w 2))]))

(define (merge-sizes surviving disappearing)
  (match* (surviving disappearing)
    [((relative-size a) (relative-size b)) (relative-size (+ a b))]
    [(_ _) surviving]))

(define (windows-for-buffer editor buffer)
  (map car (filter (lambda (e) (eq? (window-buffer (car e)) buffer))
                   (circular-list->list (editor-windows editor)))))

(define (window-for-buffer editor buffer)
  (define ws (windows-for-buffer editor buffer))
  (and (pair? ws) (car ws)))

(define (entry-for? window) (lambda (e) (eq? (car e) window)))

(define (invalidate-layout! editor)
  (set-editor-layout! editor #f))

(define (layout! editor)
  (when (not (editor-layout editor))
    (set-editor-layout! editor (layout-windows (circular-list->list (editor-windows editor))
                                               (editor-mini-window editor)
                                               (tty-columns (editor-tty editor))
                                               (tty-rows (editor-tty editor)))))
  (editor-layout editor))

(define (window-layout editor win)
  (cond [(memf (lambda (l) (eq? (layout-window l) win)) (layout! editor)) => car]
        [else #f]))

(define ((-layout-accessor- getter) editor window)
  (cond [(window-layout editor window) => getter]
        [else #f]))

(define window-size-spec (-layout-accessor- layout-size-spec))

(define (update-window-entry editor win updater)
  (set-editor-windows! editor (circular-list-replacef (editor-windows editor)
                                                      (entry-for? win)
                                                      updater))
  (invalidate-layout! editor))

(define (open-window editor buffer
                     #:after-window [after-window (editor-active-window editor)]
                     #:proportional? [proportional? #f]
                     #:activate? [activate? #t])
  (define existing-w (window-for-buffer editor buffer))
  (define existing-size (window-size-spec editor after-window))
  (define new-size (if proportional? existing-size (split-size existing-size)))
  (define new-point (or (and existing-w (buffer-mark-pos* buffer (window-point existing-w))) 0))
  (define new-window (make-window buffer #:point new-point))
  (update-window-entry editor after-window
                       (lambda (e) (list (list after-window new-size)
                                         (list new-window new-size))))
  (when activate? (set-editor-active-window! editor new-window))
  new-window)

(define (close-other-windows editor win)
  (for ((entry (circular-list->list (editor-windows editor))) #:when (not (eq? (car entry) win)))
    (set-window-buffer! (car entry) #f))
  (set-editor-windows! editor (list->circular-list (list (list win (relative-size 1)))))
  (set-editor-active-window! editor win)
  (invalidate-layout! editor))

(define (close-window editor win)
  (define prev (editor-prev-window editor win))
  (define prev-size (window-size-spec editor prev))
  (define win-size (window-size-spec editor win))
  (when (and prev (> (circular-length (editor-windows editor)) 1))
    (when (eq? (editor-active-window editor) win) (set-editor-active-window! editor prev))
    (update-window-entry editor win (lambda (e) '()))
    (resize-window editor prev (merge-sizes prev-size win-size))))

(define (resize-window editor win size)
  (update-window-entry editor win (lambda (e) (list (list win size)))))

(define (select-window editor win)
  (when (window-layout editor win)
    (set-editor-active-window! editor win)))

(define (visit-file! editor filename)
  (set-window-buffer! (editor-active-window editor)
                      (configure-fresh-buffer! editor
                                               (load-buffer (editor-buffers editor)
                                                            (local-file-buffer-source filename)))))

(define (render-editor! editor)
  (render-windows! (editor-tty editor)
                   (layout! editor)
                   (editor-active-window editor)))

(define (editor-active-buffer editor)
  (define w (editor-active-window editor))
  (and w (window-buffer w)))

(define (editor-active-modeset editor)
  (let* ((b (editor-active-buffer editor))
         (b (if (eq? b (editor-echo-area editor)) (editor-recursive-edit editor) b)))
    (and b (buffer-modeset b))))

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

(define (editor-command signature editor
                        #:args args
                        #:keyseq [keyseq #f]
                        #:prefix-arg [prefix-arg '#:default])
  (window-command signature (editor-active-window editor)
                  #:args args
                  #:editor editor
                  #:keyseq keyseq
                  #:prefix-arg prefix-arg))

(define ((abort-handler editor) e)
  (message editor "~a" (exn-message e) #:duration (exn:abort-duration e))
  (void))

(define (invoke/history cmd)
  (define editor (command-editor cmd))
  (with-handlers* ([exn:abort? (abort-handler editor)])
    (let ((old-last-command (editor-last-command editor)))
      (define result (invoke cmd))
      (when (eq? (editor-last-command editor) old-last-command)
        (set-editor-last-command! editor cmd))
      result)))

(define (collect-args-and-invoke/history editor sig keyseq prefix-arg)
  (with-handlers* ([exn:abort? (abort-handler editor)])
    (collect-args sig editor (lambda (args)
                               (invoke/history (editor-command sig editor
                                                               #:args args
                                                               #:keyseq keyseq
                                                               #:prefix-arg prefix-arg))))))

(define (editor-last-command? editor . possible-signatures)
  (and (editor-last-command editor)
       (for/or ((signature (in-list possible-signatures)))
         (equal? (command-command-signature (editor-last-command editor)) signature))))

(define (root-keyseq-handler editor)
  (modeset-keyseq-handler (editor-active-modeset editor)))

(define *error-count* 0)
(define (open-debugger editor exc)
  (define error-report
    (if (exn? exc)
        (parameterize ([current-error-port (open-output-string)])
          ((error-display-handler) (exn-message exc) exc)
          (get-output-string (current-error-port)))
        (format "~v" exc)))
  (log-error "Exception:\n~a\n" error-report)
  (set! *error-count* (+ *error-count* 1))
  (when (>= *error-count* 3) (exit))
  (define b (find-buffer editor "*Error*"))
  (buffer-replace-contents! b (string->rope error-report))
  (open-window editor b))

(define-simple-command-signature (unbound-key-sequence) #:category event)

(define (editor-background-events editor result-handler)
  (apply choice-evt
         (for/list [(e (in-hash-values (editor-active-events editor)))]
           (handle-evt e result-handler))))

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
                (editor-background-events editor
                                          (lambda (wants-repaint?)
                                            (loop total-keyseq
                                                  '()
                                                  next-handler
                                                  (if wants-repaint?
                                                      (request-repaint)
                                                      next-repaint-deadline))))
                (handle-evt (tty-next-key-evt (editor-tty editor))
                            (lambda (new-key)
                              (define new-input (list new-key))
                              (clear-message editor)
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
           (when (not (invoke/history (editor-command cmd:unbound-key-sequence
                                                      editor
                                                      #:args '()
                                                      #:keyseq total-keyseq)))
             (message editor "Unbound key sequence: ~a" (keyseq->keyspec total-keyseq)))
           (loop '() '() (root-keyseq-handler editor) (request-repaint))]
          [(incomplete-key-sequence next-handler)
           (message #:log? #f editor "~a-" (keyseq->keyspec total-keyseq))
           (wait-for-input next-handler)]
          [(command-invocation sig prefix-arg remaining-input)
           (define accepted-input
             (let remove-tail ((keyseq total-keyseq))
               (if (equal? keyseq remaining-input)
                   '()
                   (cons (car keyseq) (remove-tail (cdr keyseq))))))
           (collect-args-and-invoke/history editor sig accepted-input prefix-arg)
           (loop '() remaining-input (root-keyseq-handler editor) (request-repaint))])]))))

(define (editor-request-shutdown! editor)
  (set-editor-running?! editor #f))

(define (editor-force-redisplay! editor)
  (tty-reset (editor-tty editor))
  (invalidate-layout! editor))

;; Answers #t if it waited the full length of time.
(define (editor-sit-for editor seconds)
  (define deadline (+ (current-inexact-milliseconds) (* seconds 1000.0)))

  ;; It is safe to use e (which includes tty-input-available-evt) more
  ;; than once but ONLY because we do not actually read any input from
  ;; the tty between uses.
  (define e (choice-evt (tty-input-available-evt (editor-tty editor))
                        (editor-background-events
                         editor
                         (lambda (wants-repaint?) ;; repainting handled by loop structure
                           'recheck))))

  (define input-available?
    (let recheck ()
      (match (sync/timeout 0 e)
        [#f
         (render-editor! editor)
         (define remaining (max 0 (/ (- deadline (current-inexact-milliseconds)) 1000.0)))
         (match (sync/timeout remaining e)
           [#f #f]
           [#t #t]
           ['recheck (recheck)])]
        [#t #t]
        ['recheck (recheck)])))
  (not input-available?))

(define (set-editor-event! editor key e)
  (set-editor-active-events! editor (hash-set (editor-active-events editor) key e)))

(define (clear-editor-event! editor key)
  (set-editor-active-events! editor (hash-remove (editor-active-events editor) key)))

(define (set-editor-timeout! editor key delay-ms thunk
                             #:wants-repaint? [wants-repaint? #t])
  (set-editor-event! editor key
                     (handle-evt (alarm-evt (+ (current-inexact-milliseconds) delay-ms))
                                 (lambda (_) (thunk) wants-repaint?))))

(define (clear-message editor)
  (when (positive? (buffer-size (editor-echo-area editor)))
    (buffer-replace-contents! (editor-echo-area editor) (empty-rope))
    (define re (editor-recursive-edit editor))
    (when (and re (not (eq? (window-buffer (editor-mini-window editor)) re)))
      (set-window-buffer! (editor-mini-window editor) re (buffer-size re)))
    (clear-editor-event! editor clear-message)
    (invalidate-layout! editor)))

(define (message #:duration [duration0 #f]
                 #:log? [log? #t]
                 editor fmt . args)
  (define duration (or duration0 (and (editor-recursive-edit editor) 2)))
  (define msg (string->rope (apply format fmt args)))
  (define echo-area (editor-echo-area editor))
  (when log?
    (let* ((msgbuf (find-buffer editor "*Messages*"))
           (msgwins (filter (lambda (w) (equal? (buffer-mark-pos msgbuf (window-point w))
                                                (buffer-size msgbuf)))
                            (windows-for-buffer editor msgbuf))))
      (buffer-insert! msgbuf (buffer-size msgbuf) (rope-append msg (string->rope "\n")))
      (for ((w msgwins)) (buffer-mark! msgbuf (window-point w) (buffer-size msgbuf)))))
  (buffer-replace-contents! echo-area msg)
  (set-window-buffer! (editor-mini-window editor) echo-area (buffer-size echo-area))
  (invalidate-layout! editor)
  (when duration
    (set-editor-timeout! editor clear-message (* duration 1000.0)
                         (lambda () (clear-message editor))))
  (render-editor! editor))

(define (start-recursive-edit editor buf)
  (when (editor-recursive-edit editor)
    (abort "Command attempted to use minibuffer while in minibuffer"))
  (set-editor-recursive-edit! editor buf)
  (define miniwin (editor-mini-window editor))
  (set-window-buffer! miniwin buf (buffer-size buf))
  (set-editor-windows! editor
                       (circular-snoc (editor-windows editor)
                                      (list miniwin (absolute-size 0))))
  (set-editor-active-window! editor miniwin)
  (invalidate-layout! editor))

(define (abandon-recursive-edit editor)
  (set-editor-recursive-edit! editor #f)
  (define echo-area (editor-echo-area editor))
  (define miniwin (editor-mini-window editor))
  (set-window-buffer! miniwin echo-area (buffer-size echo-area))
  (when (eq? (editor-active-window editor) miniwin)
    (set-editor-active-window! editor (car (circular-car (editor-windows editor)))))
  (update-window-entry editor miniwin (lambda (e) '()))
  (invalidate-layout! editor))

(define-local-definer define-editor-local editor-locals set-editor-locals!)

;;---------------------------------------------------------------------------

(define-simple-command-signature (save-buffers-kill-terminal))
(define-simple-command-signature (force-redisplay))
(define-simple-command-signature (keyboard-quit))
(define-simple-command-signature (dump-buffer-to-stderr))

(define-command kernel-mode cmd:save-buffers-kill-terminal (#:editor ed)
  #:bind-key "C-x C-c"
  (editor-request-shutdown! ed))

(define-command kernel-mode cmd:force-redisplay (#:editor ed)
  #:bind-key "C-l"
  #:bind-key "<window-resize>"
  (editor-force-redisplay! ed))

(define-command kernel-mode cmd:keyboard-quit ()
  #:bind-key "C-g"
  (abort "Quit"))

(define-command kernel-mode cmd:dump-buffer-to-stderr (#:buffer buf #:window win #:editor ed)
  #:bind-key "C-M-x"
  (local-require racket/pretty)
  (log-info "")
  (log-info "--------------------------------------------------------------------------------")
  (log-info "--------------------------------------------------------------------------------")
  (log-info "========================================================================= WINDOW")
  (log-info "id ~v" (window-id win))
  (log-info "top ~v ~v" (window-top win) (buffer-mark-pos* buf (window-top win)))
  (log-info "point ~v ~v" (window-point win) (buffer-mark-pos* buf (window-point win)))
  (log-info "mark ~v" (buffer-mark-pos* buf region-mark))
  (log-info "title ~v" (buffer-title buf))
  (log-info "locals:")
  (for (((k v) (in-hash (buffer-locals buf)))) (log-info" - ~a: ~v" k v))
  (log-info "rope:")
  (pretty-write (buffer-rope buf) (current-error-port))
  (log-info "modeset:")
  (pretty-write (buffer-modeset buf) (current-error-port))
  (let ((t (editor-tty ed)))
    (log-info "terminal width ~v height ~v cursor-row ~v -col ~v"
              (tty-columns t) (tty-rows t) (tty-cursor-row t) (tty-cursor-column t)))
  (log-info "editor layout:")
  (cond [(editor-layout ed) =>
         (lambda (layouts)
           (for ((l layouts))
             (match-define (layout w s tt ll) l)
             (log-info " - ~a ~v top ~a left ~a width ~a height ~a"
                       (window-id w) s tt ll (window-width w) (window-height w))))]
        [else (log-info " - not cached")])
  (log-info "editor size-specs: ~v"
            (for/list ((e (circular-list->list (editor-windows ed))))
              (list (window-id (car e)) (cadr e))))
  (log-info "--------------------------------------------------------------------------------"))
