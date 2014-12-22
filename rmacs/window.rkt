#lang racket/base

(provide (except-out (struct-out window) window)
         (struct-out absolute-size)
         (struct-out relative-size)
         make-window
         window-split
         )

(require racket/match)

(require "buffer.rkt")
(require "lists.rkt")

;; A SizeSpec is either
;; -- (absolute-size PositiveInteger), a specific size in screen rows
;; -- (relative-size PositiveReal), a weighted window size
(struct absolute-size (lines) #:prefab)
(struct relative-size (weight) #:prefab)

(struct window (id ;; Symbol
                [buffer #:mutable] ;; Buffer
                ) #:prefab)

(define (make-window initial-buffer)
  (window (gensym 'window)
          initial-buffer))

(define (scale-size s)
  (match s
    [(absolute-size _) s] ;; can't scale fixed-size windows
    [(relative-size w) (relative-size (/ w 2))]))

(define (window-split w ws #:proportional? [proportional? #f])
  (replacef ws
            (lambda (e) (eq? (car e) w))
            (lambda (e)
              (define new-size (if proportional? (cadr e) (scale-size (cadr e))))
              (list (list w new-size)
                    (list (make-window (window-buffer w)) new-size)))))
