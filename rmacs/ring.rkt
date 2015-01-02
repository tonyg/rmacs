#lang racket/base

(provide (struct-out ring)
         make-ring
         ring-push!
         ring-peek
         ring-pop!
         ring-unpush!
         )

(require racket/match)
(require "circular-list.rkt")

;; A Ring is a (ring CircularList Nat).
(struct ring ([items #:mutable]
              max-count
              ) #:prefab)

(define (make-ring #:max-count [max-count 60])
  (ring circular-empty max-count))

(define (ring-push! ring item)
  (define items (circular-cons item (ring-items ring)))
  (set-ring-items! ring (if (> (circular-length items) (ring-max-count ring))
                            (circular-butlast items)
                            items)))

(define (ring-peek ring [index 0])
  (circular-list-ref (ring-items ring) index))

(define (ring-pop! ring [index 0])
  (set-ring-items! ring (circular-list-rotate (ring-items ring) index))
  (ring-peek ring 0))

(define (ring-unpush! ring)
  (match (ring-items ring)
    [(circular-cons item rest)
     (set-ring-items! ring rest)
     item]))
