#lang racket/base

(require "buffer.rkt")

;; Finseth's book defines a C routine, Framer(), which is intended to
;; ensure that the `top_of_window` mark, denoting the position where
;; display should begin for the current window, is in a sane position.
;; The mark is left alone unless the cursor is outside the currently
;; displayed window, either above or below. If the mark needs to be
;; moved, it is moved to a line such that the cursor, after redisplay,
;; will end up at a configurable percentage of the way down the
;; window.
;;
;; MarkType Location Buffer -> Buffer
;; Ensures the given mark is sanely positioned as a top-of-window mark
;; with respect to the given cursor position.
(define (frame-buffer! top-of-window-mtype cursor-position window-height buf
                       #:preferred-position-fraction [preferred-position-fraction 1/2])
  (define old-top-of-window-pos (buffer-mark-pos buf top-of-window-mtype))
  (define preferred-distance-from-bottom (ceiling (* window-height (- 1 preferred-position-fraction))))
  (let loop ((pos (buffer-find buf "\n" #:forward? #f #:move? #f))
             (line-count 0)
             (top-of-window-pos old-top-of-window-pos))
    (define new-top-of-window-pos
      (if (= line-count preferred-distance-from-bottom) pos top-of-window-pos))
    (cond
     [(<= pos old-top-of-window-pos)
      buf]
     [(= line-count window-height)
      (buffer-mark! buf top-of-window-mtype #:position new-top-of-window-pos)]
     [else
      (loop (buffer-find buf "\n" #:forward? #f #:move? #f #:position (- pos 1))
            (+ line-count 1)
            new-top-of-window-pos)])))
