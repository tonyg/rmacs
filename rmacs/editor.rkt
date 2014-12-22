#lang racket/base

(provide (except-out (struct-out editor) editor)
         make-editor
         visit-file!
         render-editor!
         )

(require "buffer.rkt")
(require "display.rkt")
(require "window.rkt")
(require "render.rkt")

(struct editor (buffers ;; BufferGroup
                [windows #:mutable] ;; (List (List Window SizeSpec)), abstract window layout
                [active-window #:mutable] ;; (Option Window)
                ) #:prefab)

(define (make-editor)
  (define g (make-buffergroup))
  (define scratch (make-buffer g "*scratch*" #:initial-contents ";; This is the scratch buffer."))
  (define w (make-window scratch))
  (editor g (list (list w (relative-size 1))) w))

(define (visit-file! editor filename)
  (set-window-buffer! (editor-active-window editor)
                      (file->buffer (editor-buffers editor)
                                    filename)))

(define (render-editor! editor)
  (render-windows! (editor-windows editor)
                   (editor-active-window editor)))
