#lang racket/base

(provide (struct-out history)
         make-history
         history-length
         history-ref
         history-push!
         )

(require (only-in racket/list take))

;; A History is a (history (List String) (Option Nat) Boolean).
(struct history ([items #:mutable]
                 max-count
                 delete-duplicates?
                 ) #:prefab)

(define (make-history [items '()]
                      #:max-count [max-count 30]
                      #:delete-duplicates? [delete-duplicates? #f])
  (history items max-count delete-duplicates?))

(define (history-length h)
  (length (history-items h)))

(define (history-ref h index)
  (and (>= index 0)
       (< index (history-length h))
       (list-ref (history-items h) index)))

(define (history-push! h item)
  (define new-items
    (cons item (if (history-delete-duplicates? h)
                   (filter (lambda (x) (not (equal? x item))) (history-items h))
                   (history-items h))))
  (define limit (history-max-count h))
  (set-history-items! h (if (> (length new-items) limit)
                            (take new-items limit)
                            new-items)))
