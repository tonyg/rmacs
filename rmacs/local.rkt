#lang racket/base

(provide make-locals
         make-local
         define-local-definer)

;; A LocalsTable is a (HashEqTable Symbol Any).

(define (make-locals) (hasheq))

(define (make-local get-locals set-locals! name [default #f])
  (case-lambda
    [(thing)
     (hash-ref (get-locals thing) name (lambda () default))]
    [(thing val)
     (set-locals! thing (if (equal? val default)
                            (hash-remove (get-locals thing) name)
                            (hash-set (get-locals thing) name val)))
     val]))

(define-syntax-rule (define-local-definer definer get-locals set-locals!)
  (define-syntax definer
    (syntax-rules ()
      ((_ name) (define name (make-local get-locals set-locals! 'name)))
      ((_ name default) (define name (make-local get-locals set-locals! 'name default))))))
