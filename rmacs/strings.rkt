#lang racket/base
;; String utilities :-(

(provide string-prefix?)

(define (string-prefix? a b [string=? string=?])
  (define a-len (string-length a))
  (and (>= (string-length b) a-len)
       (string=? (substring b 0 a-len) a)))

(module+ test
  (require rackunit)
  (check-true (string-prefix? "aaa" "aaaa"))
  (check-false (string-prefix? "aaaa" "aaa"))
  (check-false (string-prefix? "a" "z"))
  (check-false (string-prefix? "z" "a"))
  (check-true (string-prefix? "a" "a"))
  )
