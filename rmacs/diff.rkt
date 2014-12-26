#lang racket/base
;; Text diff algorithm following Hunt and McIlroy 1976.
;;
;; J. W. Hunt and M. D. McIlroy, An algorithm for differential file
;; comparison, Bell Telephone Laboratories CSTR #41 (1976)
;; http://www.cs.dartmouth.edu/~doug/

(provide diff-indices
         apply-patch!)

(require racket/set)
(require racket/match)
(require (only-in racket/sequence sequence-length))

(define (equivalence-classes xs)
  (for/fold [(classes (hash))]
            [(i (in-naturals))
             (item xs)]
    (hash-set classes item (cons i (hash-ref classes item '())))))

(struct candidate (x-index y-index chain) #:prefab)

(define (longest-common-subsequence xs ys)
  (define ys-equivalence-classes (equivalence-classes ys))
  (define candidates (hash 0 (candidate -1 -1 #f)))
  (for [(i (in-naturals))
        (item xs)]
    (define r 0)
    (define c (hash-ref candidates 0))
    (let/ec break
      (for ((j (in-list (reverse (hash-ref ys-equivalence-classes item '())))))
        ;; j names an index into ys
        (define s (let loop ((s r))
                    (cond
                     [(= s (hash-count candidates)) s]
                     [(and (< (candidate-y-index (hash-ref candidates s)) j)
                           (or (= s (- (hash-count candidates) 1))
                               (> (candidate-y-index (hash-ref candidates (+ s 1))) j))) s]
                     [else (loop (+ s 1))])))
        (when (< s (hash-count candidates))
          (define new-candidate (candidate i j (hash-ref candidates s)))
          (set! candidates (hash-set candidates r c))
          (set! r (+ s 1))
          (set! c new-candidate)
          (when (= r (hash-count candidates))
            ;; no point in examining further js
            (break (void))))))
    (set! candidates (hash-set candidates r c)))
  ;; At this point, we know the LCS: it's in the reverse of the
  ;; linked-list through `candidate-chain` of (hash-ref candidates (-
  ;; (hash-count candidates) 1)).
  (reverse
   (let loop ((c (hash-ref candidates (- (hash-count candidates) 1))))
     (if (candidate-chain c)
         (cons (cons (candidate-x-index c) (candidate-y-index c))
               (loop (candidate-chain c)))
         '()))))

(define (diff-indices xs ys)
  (let loop ((i -1)
             (j -1)
             (matches (append (longest-common-subsequence xs ys)
                              (list (cons (sequence-length xs) (sequence-length ys))))))
    (match matches
      ['() '()]
      [(cons (cons mi mj) rest)
       (define li (- mi i 1))
       (define lj (- mj j 1))
       (if (or (positive? li) (positive? lj))
           (cons (list (+ i 1) li (+ j 1) lj) (loop mi mj rest))
           (loop mi mj rest))])))

;; patch-indices is a result from a call to diff-indices
(define (apply-patch! patch-indices ;; DiffIndices
                      remove-elements! ;; Nat Nat -> Void
                      insert-elements! ;; Nat Nat Nat -> Void
                      )
  (for/fold [(skew 0)] [(patch patch-indices)]
    (match-define (list old-i old-n new-i new-n) patch)
    (define delta (- new-n old-n))
    (if (negative? delta)
        (begin (remove-elements! (+ old-i skew) (- delta))
               (+ skew delta))
        skew))
  (for/fold [(skew 0)] [(patch patch-indices)]
    (match-define (list old-i old-n new-i new-n) patch)
    (define delta (- new-n old-n))
    (insert-elements! (+ old-i skew) (max 0 delta) new-n)
    (+ skew delta))
  (void))

(module+ test
  (require rackunit)

  ;; (define (test-example xs ys)
  ;;   (printf "~v\n" (longest-common-subsequence xs ys))
  ;;   (printf "~v\n" (diff-indices xs ys)))
  ;; (test-example "The red brown fox jumped over the rolling log"
  ;;               "The brown spotted fox leaped over the rolling log")

  (check-equal? (diff-indices "The red brown fox jumped over the rolling log"
                              "The brown spotted fox leaped over the rolling log")
                '((4 4 4 0) (14 0 10 8) (18 3 22 3)))

  (check-equal? (longest-common-subsequence "acbcaca" "bcbcacb")
                '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5)))
  (check-equal? (longest-common-subsequence "bcbcacb" "acbcaca")
                '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5)))
  (check-equal? (longest-common-subsequence "acba" "bcbb")
                '((1 . 1) (2 . 2)))
  (check-equal? (longest-common-subsequence "abcabba" "cbabac")
                '((2 . 0) (3 . 2) (4 . 3) (6 . 4)))
  (check-equal? (longest-common-subsequence "cbabac" "abcabba")
                '((1 . 1) (2 . 3) (3 . 4) (4 . 6)))

  (check-equal? (longest-common-subsequence
                 (vector (vector 1 1 1) (vector 1 1 1) (vector 1 1 1) (vector 1 1 1))
                 (vector (vector 1 1 1) (vector 2 2 2) (vector 1 1 1) (vector 4 4 4)))
                '((0 . 0) (1 . 2)))
  (check-equal? (diff-indices
                 (vector (vector 1 1 1) (vector 1 1 1) (vector 1 1 1) (vector 1 1 1))
                 (vector (vector 1 1 1) (vector 2 2 2) (vector 1 1 1) (vector 4 4 4)))
                '((1 0 1 1) (2 2 3 1)))

  (check-equal? (longest-common-subsequence '(a b c) '(d e f)) '())
  (check-equal? (diff-indices '(a b c) '(d e f)) '((0 3 0 3))))
