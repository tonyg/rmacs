#lang racket/base

(provide (struct-out ring)
         make-ring
         ring-ref
         ring-rotate!
         ring-add-item!
         ring-remove-item!
         )

(require racket/match)
(require "circular-list.rkt")

;; A Ring is a (ring CircularList Nat).
(struct ring ([items #:mutable]
              max-count
              ) #:prefab)

(define (make-ring [items circular-empty] #:max-count [max-count 60])
  (ring items max-count))

(define (ring-ref ring [index 0])
  (circular-list-ref (ring-items ring) index))

(define (ring-rotate! ring [count 0])
  (set-ring-items! ring (circular-list-rotate (ring-items ring) count)))

(define (ring-add-item! ring item)
  (define items (circular-cons item (ring-items ring)))
  (set-ring-items! ring (if (> (circular-length items) (ring-max-count ring))
                            (circular-butlast items)
                            items)))

(define (ring-remove-item! ring)
  (match (ring-items ring)
    [(circular-cons item rest)
     (set-ring-items! ring rest)
     item]))
