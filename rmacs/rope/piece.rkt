#lang racket/base
;; A Piece is a value carried by a particular Rope node.
;; Each Piece has a size associated with it - it takes up that many positions in the rope.

(provide gen:piece
         piece?
         piece-size
         piece-empty?
         piece->searchable-string
         piece-match
         piece-split
         piece-merge
         piece-rev-merge
         piece->index

         *strand:glom-up-to*
         (struct-out strand)
         strand-empty
         strand-empty?
         string->strand
         strand->string
         substrand
         strand-equal?

         define/generic ;; for convenience
         )

(require racket/generic)
(require racket/match)
(require (only-in racket/bool false?))
(require "range.rkt")

(define-generics piece
  (piece-size piece)
  (piece-empty? piece)
  (piece->searchable-string piece) ;; (string-length RESULT) === (piece-size piece) !
  (piece-match piece forward? key offset)
  (piece-split piece offset)
  (piece-merge piece piece2 k-merge k-no-merge)
  (piece-rev-merge piece piece2 k-merge k-no-merge)
  (piece->index piece)
  #:defaults ([string?
               (define/generic rev-merge piece-rev-merge)
               (define (piece-size s) (string-length s))
               (define (piece-empty? s) (zero? (string-length s)))
               (define (piece->searchable-string s) s)
               (define (piece-match s forward? key offset) '())
               (define (piece-split s offset) (values (substrand s 0 offset) (substrand s offset)))
               (define (piece-merge s1 s2 k-merge k-no-merge)
                 (if (string? s2)
                     (if (<= (+ (string-length s1) (string-length s2)) *strand:glom-up-to*)
                         (k-merge (string-append s1 s2))
                         (k-no-merge))
                     (rev-merge s2 s1 k-merge k-no-merge)))
               (define (piece-rev-merge s2 s1 k-merge k-no-merge)
                 (k-no-merge))
               (define (piece->index s) #f)]

              [false?
               (define (piece-size s) 0)
               (define (piece-empty? s) #t)
               (define (piece->searchable-string s) "")
               (define (piece-match s forward? key offset) '())
               (define (piece-split s offset) (values #f #f))
               (define (piece-merge s1 s2 k-merge k-no-merge)
                 (k-merge s2))
               (define (piece-rev-merge s2 s1 k-merge k-no-merge)
                 (k-merge s2))
               (define (piece->index s) #f)]))

;; TODO: measure to see if usage of *strand:glom-up-to* improves or worsens memory usage
(define *strand:glom-up-to* 128)

;; A Strand is a (strand String Number Number), representing a
;; substring of a string.
(struct strand (text offset length) #:transparent
  #:methods gen:piece
  [(define/generic rev-merge piece-rev-merge)
   (define (piece-size s) (strand-length s))
   (define (piece-empty? s) (strand-empty? s))
   (define (piece->searchable-string s) (strand->string s))
   (define (piece-match s forward? key offset) '())
   (define (piece-split s offset) (values (substrand s 0 offset) (substrand s offset)))
   (define (piece-merge s1 s2 k-merge k-no-merge)
     (cond [(string? s2) (strand-merge s1 (string->strand s2) k-merge k-no-merge)]
           [(strand? s2) (strand-merge s1 s2 k-merge k-no-merge)]
           [else (rev-merge s2 s1 k-merge k-no-merge)]))
   (define (piece-rev-merge s2 s1 k-merge k-no-merge)
     (cond [(string? s1) (strand-merge (string->strand s1) s2 k-merge k-no-merge)]
           [(strand? s1) (strand-merge s1 s2 k-merge k-no-merge)]
           [else (k-no-merge)]))
   (define (piece->index s) #f)])

(define (strand-empty) (strand "" 0 0))
(define (strand-empty? s) (zero? (strand-length s)))

(define (string->strand s) (strand s 0 (string-length s)))

(define (strand->string s)
  (match-define (strand text offset length) s)
  (if (= length (string-length text))
      text
      (substring text offset (+ offset length))))

(define (substrand t0 [lo0 #f] [hi0 #f])
  (define t (if (string? t0) (string->strand t0) t0))
  (define-values (lo hi) (compute-range-lo+hi lo0 hi0 (strand-length t)))
  (strand (strand-text t)
          (+ (strand-offset t) lo)
          (- hi lo)))

(define (strand-equal? t1 t2)
  (string=? (strand->string t1)
            (strand->string t2)))

(define (strand-merge t1 t2 k-merge k-no-merge)
  (match-define (strand text1 offset1 count1) t1)
  (match-define (strand text2 offset2 count2) t2)
  (cond [(zero? count1) (k-merge t2)]
        [(zero? count2) (k-merge t1)]
        [(and (eq? text1 text2)
              (= (+ offset1 count1) offset2))
         (if (and (= offset1 0) (= (+ offset2 count2) (string-length text1)))
             (k-merge text1)
             (k-merge (strand text1 offset1 (+ count1 count2))))]
        [(< (+ count1 count2) *strand:glom-up-to*)
         (k-merge (string-append (strand->string t1) (strand->string t2)))]
        [else (k-no-merge)]))
