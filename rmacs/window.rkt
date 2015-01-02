#lang racket/base

(provide (except-out (struct-out window) window set-window-buffer!)
         (rename-out [set-window-buffer!* set-window-buffer!])
         make-window
         window-editor
         window-command
         window-move-to!
         position-visible?
         )

(require racket/match)

(require "buffer.rkt")
(require "rope.rkt")

(struct window (id ;; Symbol
                top ;; MarkType
                bottom ;; MarkType
                point ;; MarkType
                [buffer #:mutable] ;; (Option Buffer)
                [status-line? #:mutable] ;; Boolean
                ) #:prefab)

(define (make-window initial-buffer #:point [initial-point-or-mark 0])
  (define id (gensym 'window))
  (define w (window id
                    (mark-type (buffer-mark-type 'top id #f) 'left)
                    (mark-type (buffer-mark-type 'bottom id #f) 'left)
                    (mark-type (buffer-mark-type 'point id #t) 'right)
                    #f
                    #t))
  (set-window-buffer!* w initial-buffer initial-point-or-mark) ;; sets initial marks
  w)

(define (window-editor w)
  (and (window-buffer w)
       (buffer-editor (window-buffer w))))

(define (set-window-buffer!* win new [point-or-mark 0])
  (define old (window-buffer win))
  (when old
    (buffer-clear-mark! old (window-top win))
    (buffer-clear-mark! old (window-bottom win))
    (buffer-clear-mark! old (window-point win)))
  (set-window-buffer! win new)
  (when new
    (buffer-mark! new (window-point win) point-or-mark))
  (void))

(define (window-command selector window
                        #:editor [editor #f]
                        #:keyseq [keyseq #f]
                        #:prefix-arg [prefix-arg '#:default])
  (command selector (window-buffer window)
           #:window window
           #:editor editor
           #:keyseq keyseq
           #:prefix-arg prefix-arg))

(define (window-move-to! win pos)
  (buffer-mark! (window-buffer win) (window-point win) pos)
  win)

;; NOTE: relies on the frame being accurate, so it is not valid to
;; rely on the results of this call if any change has been made to the
;; buffer since the last re-framing (i.e. usually the last redisplay).
(define (position-visible? win pos)
  (define t (buffer-mark-pos* (window-buffer win) (window-top win)))
  (define b (buffer-mark-pos* (window-buffer win) (window-bottom win)))
  (and t b (<= t pos b)))
