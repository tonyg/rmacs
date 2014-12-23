#lang racket/base

(provide topsort)

(require racket/match)

(define (topsort edges
                 #:comparison [comparison equal?])
  (define hash-ctor (cond [(eq? comparison equal?) hash]
                          [(eq? comparison eq?) hasheq]
                          [else (error 'topsort "Invalid comparison ~v" comparison)]))
  (define-values (fwd rev)
    (for/fold [(fwd (hash-ctor)) (rev (hash-ctor))]
              [(edge edges)]
      (match-define (list source target) edge)
      (values (hash-set fwd source (hash-set (hash-ref fwd source hash-ctor) target #t))
              (hash-set rev target (hash-set (hash-ref rev target hash-ctor) source #t)))))
  (define roots (for/fold [(roots (hash-ctor))]
                          [(source (in-hash-keys fwd))]
                  (if (hash-has-key? rev source)
                      roots
                      (hash-set roots source #t))))

  (if (hash-empty? roots)
      (if (and (hash-empty? fwd) (hash-empty? rev))
          '() ;; no nodes at all
          #f) ;; no nodes without incoming edges -> cycle
      (let/ec return
        (define seen (hash-ctor))
        (define busy (hash-ctor))
        (define acc '())

        (define (visit-nodes nodes)
          (for ((n nodes))
            (when (hash-has-key? busy n) (return #f)) ;; cycle
            (when (not (hash-has-key? seen n))
              (set! busy (hash-set busy n #t))
              (visit-nodes (hash-keys (hash-ref fwd n hash-ctor)))
              (set! seen (hash-set seen n #t))
              (set! busy (hash-remove busy n))
              (set! acc (cons n acc)))))

        (visit-nodes (hash-keys roots))
        acc)))

(module+ test
  (require rackunit)
  (check-equal? (topsort '()) '())
  (check-equal? (topsort '((1 1))) #f)
  (check-equal? (topsort '((1 0) (0 1))) #f)
  (check-equal? (topsort '((1 2) (1 3) (3 2) (3 4) (4 0) (0 1))) #f)
  (check-equal? (topsort '((1 2) (1 3) (3 2) (3 4) (4 1) (0 1))) #f)
  (check-equal? (topsort '((1 2) (1 3) (3 2) (3 4) (0 1))) '(0 1 3 4 2)) ;; others also valid
  )
