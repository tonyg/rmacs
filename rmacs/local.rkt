#lang racket/base

(provide make-locals
         make-local
         define-local-definer)

(require racket/match)

;; A LocalsTable is a (HashEqTable Symbol Any).

(define (make-locals) (hasheq))

(define (make-local get-locals set-locals! name [default-thunk (lambda () #f)])
  (case-lambda
    [(thing)
     (hash-ref (get-locals thing) name (lambda ()
                                         (define val (default-thunk))
                                         (set-locals! thing (hash-set (get-locals thing)
                                                                      name
                                                                      val))
                                         val))]
    [(thing val)
     (match thing
       ['#:clear (set-locals! val (hash-remove (get-locals val) name))]
       [_ (set-locals! thing (hash-set (get-locals thing) name val))])
     val]))

(define-syntax-rule (define-local-definer definer get-locals set-locals!)
  (define-syntax definer
    (syntax-rules ()
      ((_ name) (define name (make-local get-locals set-locals! 'name)))
      ((_ name default) (define name (make-local get-locals
                                                 set-locals!
                                                 'name
                                                 (lambda () default)))))))
