#lang racket/base

(provide circular-list?
         circular-empty
         circular-null?
         circular-pair?
         circular-cons
         circular-car
         circular-cdr
         circular-length
         circular-list-rotate-forward
         circular-list-rotate-backward
         list->circular-list
         circular-list->list
         circular-list-map
         circular-list-filter
         circular-list-remove
         circular-list-memf)

(require racket/match)

(struct circular-list ([front #:mutable]
                       [back #:mutable]
                       ) #:prefab)

(define circular-empty (circular-list '() '()))

(define (circular-null? xs)
  (equal? xs circular-empty))

(define (circular-pair? xs)
  (and (circular-list? xs)
       (not (circular-null? xs))))

(define (circular-cons->cons xs)
  (cons (circular-car xs)
        (circular-cdr xs)))

(define (circular-cons* x xs)
  (circular-list (cons x (circular-list-front xs)) (circular-list-back xs)))

(define-match-expander circular-cons
  (syntax-rules ()
    [(_ a d) (? circular-pair? (app circular-cons->cons (cons a d)))])
  (syntax-rules ()
    [(_ a d) (circular-cons* a d)]))

(define (prime! xs)
  (match xs
    [(circular-list '() back)
     (set-circular-list-front! xs (reverse back))
     (set-circular-list-back! xs '())]
    [_ (void)])
  xs)

(define (anti-prime! xs)
  (match xs
    [(circular-list front '())
     (set-circular-list-front! xs '())
     (set-circular-list-back! xs (reverse front))]
    [_ (void)])
  xs)

(define (circular-car xs)
  (if (circular-null? xs)
      (error 'circular-car "Empty circular list")
      (car (circular-list-front (prime! xs)))))

(define (circular-cdr xs)
  (if (circular-null? xs)
      (error 'circular-cdr "Empty circular list")
      (begin (prime! xs)
             (circular-list (cdr (circular-list-front xs)) (circular-list-back xs)))))

(define (circular-length xs)
  (+ (length (circular-list-front xs))
     (length (circular-list-back xs))))

(define (circular-list-rotate-forward xs)
  (if (circular-null? xs)
      xs
      (begin (prime! xs)
             (circular-list (cdr (circular-list-front xs))
                            (cons (car (circular-list-front xs))
                                  (circular-list-back xs))))))

(define (circular-list-rotate-backward xs)
  (if (circular-null? xs)
      xs
      (begin (anti-prime! xs)
             (circular-list (cons (car (circular-list-back xs))
                                  (circular-list-front xs))
                            (cdr (circular-list-back xs))))))

(define (list->circular-list xs)
  (circular-list xs '()))

(define (circular-list->list xs)
  (append (circular-list-front xs) (reverse (circular-list-back xs))))

(define (map/reversed-order f xs)
  (if (null? xs)
      '()
      (let ((tail (map/reversed-order f (cdr xs))))
        (cons (f (car xs)) tail))))

(define (circular-list-map f xs)
  (circular-list (map f (circular-list-front xs))
                 (map/reversed-order f (circular-list-back xs))))

;; WARNING: does not preserve order of evaluation wrt back
(define (circular-list-filter f xs)
  (circular-list (filter f (circular-list-front xs))
                 (filter f (circular-list-back xs))))

(define (circular-list-remove item xs [comparison equal?])
  (define new-front (remove item (circular-list-front xs) comparison))
  (if (= (length new-front) (length (circular-list-front xs)))
      (circular-list (circular-list-front xs)
                     (reverse (remove item (reverse (circular-list-back xs)) comparison)))
      (circular-list new-front (circular-list-back xs))))

(define (circular-list-memf f xs)
  (let loop ((seen '()) (xs xs))
    (if (circular-null? xs)
        #f
        (let ((a (circular-car xs)))
          (if (f a)
              (circular-list (circular-list-front xs)
                             (append seen (circular-list-back xs)))
              (loop (cons a seen) (circular-cdr xs)))))))

(module+ test
  (require rackunit)

  (define (check-abcdef abcdef)
    (define bcdefa (circular-list-rotate-forward abcdef))
    (check-equal? (circular-length abcdef) 6)
    (check-equal? (circular-list->list abcdef) '(a b c d e f))
    (check-equal? (circular-list->list bcdefa) '(b c d e f a))
    (check-equal? (circular-list->list (for/fold [(xs abcdef)] [(i (circular-length abcdef))]
                                         (circular-list-rotate-forward xs)))
                  (circular-list->list abcdef))
    (check-equal? (circular-list->list (for/fold [(xs abcdef)] [(i (circular-length abcdef))]
                                         (circular-list-rotate-backward xs)))
                  (circular-list->list abcdef)))

  (check-abcdef (circular-list '(a b c) '(f e d)))
  (check-abcdef (circular-list '(a b c d e f) '()))
  (check-abcdef (circular-list '() '(f e d c b a)))

  (check-equal? (match (circular-cons 1 circular-empty)
                  [(circular-cons a d) (cons a d)])
                (cons 1 circular-empty))
  (check-equal? (match (circular-list-rotate-forward (circular-cons 1 circular-empty))
                  [(circular-cons a d) (cons a d)])
                (cons 1 circular-empty))
  (check-equal? (match (circular-list-rotate-forward
                        (circular-cons 1 (circular-cons 2 circular-empty)))
                  [(circular-cons a d) (cons a (circular-list->list d))])
                (list 2 1))
  (check-equal? (circular-list->list (circular-list-remove 2 (circular-list '(1 2 3) '(6 5 4))))
                '(1 3 4 5 6))
  (check-equal? (circular-list->list (circular-list-remove 2 (circular-list '(1) '(6 5 4 3 2))))
                '(1 3 4 5 6))
  (check-equal? (circular-list->list (circular-list-remove 2 (circular-list '(1 2 3 2) '(6 5 2 4))))
                '(1 3 2 4 2 5 6))
  (check-equal? (circular-list->list (circular-list-remove 2 (circular-list '(1) '(6 5 2 4 2 3 2))))
                '(1 3 2 4 2 5 6)))

