#lang racket/base

(provide (except-out (struct-out window) window set-window-buffer!)
         (rename-out [set-window-buffer!* set-window-buffer!])
         make-window
         window-command
         window-mark!
         window-move-to!
         )

(require racket/match)

(require "buffer.rkt")
(require "rope.rkt")

(struct window (id ;; Symbol
                top ;; MarkType
                point ;; MarkType
                mark ;; MarkType
                [buffer #:mutable] ;; (Option Buffer)
                ) #:prefab)

(define (make-window initial-buffer #:point [initial-point-or-mark 0])
  (define id (gensym 'window))
  (define w (window id
                    (mark-type (buffer-mark-type 'top id #f) 'left)
                    (mark-type (buffer-mark-type 'point id #t) 'right)
                    (mark-type (buffer-mark-type 'mark id #f) 'left)
                    #f))
  (set-window-buffer!* w initial-buffer initial-point-or-mark) ;; sets initial marks
  w)

(define (set-window-buffer!* win new [point-or-mark 0])
  (define old (window-buffer win))
  (when old
    (buffer-clear-mark! old (window-top win))
    (buffer-clear-mark! old (window-point win))
    (buffer-clear-mark! old (window-mark win)))
  (set-window-buffer! win new)
  (when new
    (buffer-mark! new (window-point win) point-or-mark))
  (void))

(define (window-command selector window
                        #:keyseq [keyseq #f]
                        #:prefix-arg [prefix-arg '#:default])
  (command selector (window-buffer window) #:window window #:keyseq keyseq #:prefix-arg prefix-arg))

(define (window-mark! win [pos (window-point win)])
  (buffer-mark! (window-buffer win) (window-mark win) pos)
  win)

(define (window-move-to! win pos)
  (buffer-mark! (window-buffer win) (window-point win) pos)
  win)
