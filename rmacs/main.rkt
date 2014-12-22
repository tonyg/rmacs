#lang racket/base

(require "editor.rkt")
(require "render.rkt")
(require racket/match)

(define (main)
  (with-handlers ([exn? (lambda (e)
                          (local-require ansi)
                          (tty-restore!)
                          (raise e))])
    (define e (make-editor))
    (visit-file! e (build-path (collection-file-path "main.rkt" "rmacs")
                               'up 'up "doc" "xterm_controls.txt"))
    (render-editor! e))
  (sleep 2))

(module+ main
  (void (main)))
