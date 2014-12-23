#lang racket/base

(require racket/match)

(require "editor.rkt")
(require "buffer.rkt")
(require "mode/fundamental.rkt")

(define (main)
  (define e (make-editor))
  (visit-file! e (build-path (collection-file-path "main.rkt" "rmacs")
                             'up 'up "doc" "xterm_controls.txt"))
  (buffer-add-mode! (current-editor-buffer e) fundamental-mode)
  (editor-mainloop e))

(module+ main
  (void (main)))
