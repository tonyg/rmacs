#lang racket/base
;; Pythonesque index computations including #f for default, and negative to count "from the end"

(provide compute-range-index
         compute-range-lo+hi)

(define (compute-range-index index default limit)
  (cond [(not index) default]
        [(zero? limit) 0]
        [else (max 0 (min limit (if (negative? index) (+ index limit) index)))]))

(define (compute-range-lo+hi lo hi limit)
  (values (compute-range-index lo 0 limit)
          (compute-range-index hi limit limit)))
