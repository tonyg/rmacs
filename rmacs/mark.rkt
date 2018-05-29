#lang racket/base
;; Marks and their indexes, to be used in ropes.

(provide (struct-out mark-type)
         rope-marks
         clear-mark
         set-mark
         replace-mark
         find-all-marks/type
         clear-all-marks
         clear-all-marks/type)

(require racket/set)
(require racket/match)
(require "rope.rkt")
(require "rope/piece.rkt")
(require "rope/index.rkt")

;; A MarkType is a (mark-type Any Stickiness). MarkTypes can be
;; associated with a set of Any values at each position in the rope.
(struct mark-type (info stickiness) #:prefab)

(struct marks-index (set) #:transparent
  #:methods gen:index
  [(define/generic rev-merge index-rev-merge)
   (define (index-merge i1 i2)
     (if (marks-index? i2)
         (marks-index (set-union (marks-index-set i1) (marks-index-set i2)))
         (rev-merge i2 i1)))
   (define (index-rev-merge i2 i1)
     (error 'index-rev-merge "Cannot rev-merge index ~v with ~v" i2 i1))
   (define (index-contains? i key)
     (set-member? (marks-index-set i) key))])

(struct marks (table) #:transparent
  #:methods gen:piece
  [(define/generic rev-merge piece-rev-merge)
   (define (piece-size ms) 0)
   (define (piece-empty? ms) (hash-empty? (marks-table ms)))
   (define (piece->searchable-string ms) "")
   (define (piece-match ms forward? key offset)
     (match (hash-ref (marks-table ms) key #f)
       [#f '()]
       [value (list (cons 0 value))]))
   (define (piece-split ms offset)
     (values (marks (for/hasheq [((mtype value) (in-hash (marks-table ms)))
                                 #:when (eq? (mark-type-stickiness mtype) 'left)]
                      (values mtype value)))
             (marks (for/hasheq [((mtype value) (in-hash (marks-table ms)))
                                 #:when (eq? (mark-type-stickiness mtype) 'right)]
                      (values mtype value)))))
   (define (piece-merge ms1 ms2 k-merge k-no-merge)
     (if (marks? ms2)
         (k-merge (marks (for/fold [(t (marks-table ms1))]
                                   [((mtype value) (in-hash (marks-table ms2)))]
                           (hash-set t mtype value))))
         (rev-merge ms2 ms1 k-merge k-no-merge)))
   (define (piece-rev-merge ms2 ms1 k-merge k-no-merge)
     (k-no-merge))
   (define (piece->index ms)
     (marks-index (list->seteq (hash-keys (marks-table ms)))))])

(define (rope-marks r)
  (define i (rope-index r))
  (if i (marks-index-set i) (seteq)))

(define (remove-single-mark r mtype)
  (define table (hash-remove (marks-table (rope-piece r)) mtype))
  (update-piece r (marks table)))

(define (clear-mark r0 mtype position)
  (define-values (l r) (rope-split r0 position))
  (rope-append (if (and l (marks? (rope-piece l)) (eq? (mark-type-stickiness mtype) 'left))
                   (remove-single-mark l mtype)
                   l)
               (if (and r (marks? (rope-piece r)) (eq? (mark-type-stickiness mtype) 'right))
                   (remove-single-mark r mtype)
                   r)))

(define (set-mark r0 mtype position value)
  (define-values (l r) (rope-split (clear-mark r0 mtype position) position))
  (rope-append (rope-append l (piece->rope (marks (hasheq mtype value)))) r))

(define (replace-mark r0 mtype new-pos new-value)
  (define pos (find-pos-in-index r0 mtype))
  (set-mark (if pos (clear-mark r0 mtype pos) r0) mtype new-pos new-value))

(define (find-all-marks/type r mtype)
  (find-all-in-index r mtype))

(define (clear-all-marks r)
  (rope-map r (lambda (p) (and (string? p) p))))

(define (clear-all-marks/type r mtype)
  (rope-map/key r mtype (lambda (p) (if (not (marks? p))
                                        p
                                        (marks (hash-remove (marks-table p) mtype))))))

(module+ for-test
  (provide (struct-out marks)
           (struct-out marks-index)))
