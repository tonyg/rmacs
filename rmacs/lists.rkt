#lang racket/base
;; List utilities :-(

(provide replacef)

(require racket/list)

(define (replacef lst finder replacer)
  (define-values (head tail) (splitf-at lst (lambda (e) (not (finder e)))))
  (if (null? tail)
      head
      (append head
              (replacer (car tail))
              (cdr tail))))

(module+ test
  (require rackunit)

  (check-equal? (replacef '(1 2 3 4 5) even? (lambda (n) (list n n n)))
                '(1 2 2 2 3 4 5)))
