#lang racket/base

(require racket/match)

(require "editor.rkt")
(require "buffer.rkt")
(require "mode.rkt")
(require "mode/fundamental.rkt")

(define (main)
  (define e (make-editor #:default-modeset (modeset-add-mode kernel-modeset
                                                             fundamental-mode)))
  (visit-file! e (build-path (collection-file-path "main.rkt" "rmacs")
                             'up 'up "doc" "xterm_controls.txt"))
  (editor-mainloop e))

(module+ main
  (require racket/trace)
  (current-trace-notify (lambda (s) (log-info "TRACE: ~a" s)))
  (void (main))
  ;; (require profile)
  ;; (require ansi)
  ;; (void (profile-thunk (lambda () (begin0 (main)
  ;;                                   (tty-restore!)
  ;;                                   (display (select-graphic-rendition style-normal))
  ;;                                   (display (clear-screen))
  ;;                                   (flush-output)))))
  )
