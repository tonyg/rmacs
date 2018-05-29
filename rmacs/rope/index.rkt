#lang racket/base
;; An Index is a user-maintained indexing structure associated with each Rope node.

(provide gen:index
         index?
         index-merge
         index-rev-merge
         index-contains?
         define/generic ;; for convenience
         )

(require racket/generic)
(require (only-in racket/bool false?))

(define-generics index
  (index-merge index index2)
  (index-rev-merge index index2)
  (index-contains? index key)
  #:defaults ([false?
               (define (index-merge i1 i2) i2)
               (define (index-rev-merge i2 i1) i1)
               (define (index-contains? i key) #f)]))
