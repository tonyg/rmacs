#lang racket/base
;; Ropes for text editing

;; TODO: Consider possible invariant that marks in mark-index should
;; fall within [lo,hi).

(provide empty-strand
         string->strand
         strand->string
         substrand
         strand-equal?
         strand-empty?

         (rename-out [rope?* rope?])
         rope-empty?
         empty-rope
         strand->rope
         string->rope
         rope->string

         rope-size
         rope-marks
         rope-split
         rope-append
         rope-concat
         subrope
         rope-generator
         open-input-rope
         rope-seek

         (struct-out mark-type)

         has-mark?
         find-mark
         find-mark-pos
         find-all-marks/type
         set-mark
         clear-mark
         replace-mark
         clear-all-marks

         ;; dump-mark-tree
         )

(require racket/set)
(require racket/match)
(require racket/generator)

(module+ test (require rackunit racket/pretty))

;; A Stickiness is one of
;; -- 'left or
;; -- 'right
;; and indicates the side after a rope-split to which a mark with this
;; Stickiness adheres. What Finseth calls a "normal mark" has 'right
;; stickiness, and what he calls a "fixed mark" has 'left stickiness.

;; A MarkType is a (mark-type Any Stickiness). MarkTypes can be
;; associated with a set of Any values at each position in the rope.
(struct mark-type (info stickiness) #:prefab)

;; A Strand is a (strand String Number Number), representing a
;; substring of a string.
(struct strand (text offset count) #:prefab)

;; A Marks is a (Hasheq MarkType Any), marks and their values at a
;; particular position.
(define (marks? x) (hash? x))

;; A Piece is a Strand or a Marks, the value carried by a particular
;; Rope node.

;; A Rope is a splay tree representing a long piece of text.
;; #f is the empty Rope; otherwise a (rope) struct instance.
;; INVARIANT: Adjacent ropes will be merged to maximize sharing.
(struct rope (piece ;; Piece
              left ;; Rope or #f
              right ;; Rope or #f
              size* ;; Number, total length of this rope
              marks* ;; (Seteq MarkType)
              ) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strands

(define *glom-up-to* 128)

(define (empty-strand) (strand "" 0 0))

(define (string->strand s)
  (strand s 0 (string-length s)))

(define (strand->string t)
  (match-define (strand text offset count) t)
  (if (and (zero? offset) (= count (string-length text)))
      text
      (substring text offset (+ offset count))))

(define (compute-range-index index default limit)
  (cond [(not index) default]
        [(zero? limit) 0]
        [else (max 0 (min limit (if (negative? index) (+ index limit) index)))]))

(define (substrand t0 [lo0 #f] [hi0 #f])
  (define t (if (string? t0) (string->strand t0) t0))
  (define lo (compute-range-index lo0 0 (strand-count t)))
  (define hi (compute-range-index hi0 (strand-count t) (strand-count t)))
  (strand (strand-text t)
          (+ (strand-offset t) lo)
          (- hi lo)))

(define (strand-maybe-append t1 t2)
  (match-define (strand text1 offset1 count1) t1)
  (match-define (strand text2 offset2 count2) t2)
  (or (and (zero? count1) t2)
      (and (zero? count2) t1)
      (and (eq? text1 text2)
           (= (+ offset1 count1) offset2)
           (strand text1 offset1 (+ count1 count2)))
      ;; TODO: measure to see if the following improves or worsens memory usage
      (and (< (+ count1 count2) *glom-up-to*)
           (string->strand (string-append (strand->string t1) (strand->string t2))))))

(define (strand-equal? t1 t2)
  (string=? (strand->string t1)
            (strand->string t2)))

(define (strand-empty? t)
  (and (strand? t)
       (zero? (strand-count t))))

(module+ test
  (check-equal? (strand-count (empty-strand)) 0)
  (check-equal? (strand-count (string->strand "")) 0)
  (check-true (strand-equal? (empty-strand) (string->strand ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ropes

(define (empty-rope) #f)

(define (rope-empty? r)
  (match r
    [#f #t]
    [(rope (? strand-empty?) #f #f 0 (? set-empty?)) #t]
    [_ #f]))

(define (rope?* r)
  (or (rope-empty? r)
      (rope? r)))

(define (strand->rope t)
  (rope t (empty-rope) (empty-rope) (strand-count t) (seteq)))

(define (string->rope s)
  (strand->rope (string->strand s)))

(define (rope->string r)
  (define buf (make-string (rope-size r)))
  (let fill! ((r r) (offset 0))
    (when r
      (fill! (rope-left r) offset)
      (define lo (rope-lo r))
      (match (rope-piece r)
        [(strand text text-offset text-count)
         (string-copy! buf
                       (+ offset lo)
                       text
                       text-offset
                       (+ text-offset text-count))
         (fill! (rope-right r) (+ offset lo text-count))]
        [(? marks?)
         (fill! (rope-right r) (+ offset lo))])))
  buf)

(define (replace-left r n) (if r (reindex (struct-copy rope r [left n])) n))
(define (replace-right r n) (if r (reindex (struct-copy rope r [right n])) n))
(define (replace-both r rl rr) (reindex (struct-copy rope r [left rl] [right rr])))

(define (splay-to r direction-finder arg0)
  ;; zig: last. desired position is a direct (left/right) child of r.
  ;; zig-zig: desired position is within a (left-left/right-right) grandchild of r.
  ;; zig-zag: desired position is within a (left-right/right-left) grandchild of r.
  (define-values (where arg1) (direction-finder arg0 r))
  (match where
    ['here (values arg1 r)]
    ['left
     (define rl (rope-left r))
     (define-values (where arg2) (direction-finder arg1 rl))
     (match where
       ['here ;; zig.
        (values arg2 (replace-right rl (replace-left r (and rl (rope-right rl)))))]
       ['left ;; zig-zig
        (define-values (v rll) (splay-to (rope-left rl) direction-finder arg2))
        (values v (replace-right rll (replace-both rl
                                                   (and rll (rope-right rll))
                                                   (replace-left r (rope-right rl)))))]
       ['right ;; zig-zag
        (define-values (v rlr) (splay-to (rope-right rl) direction-finder arg2))
        (values v (replace-both rlr
                                (replace-right rl (rope-left rlr))
                                (replace-left r (rope-right rlr))))])]
    ['right
     (define rr (rope-right r))
     (define-values (where arg2) (direction-finder arg1 rr))
     (match where
       ['here ;; zig.
        (values arg2 (replace-left rr (replace-right r (and rr (rope-left rr)))))]
       ['left ;; zig-zag
        (define-values (v rrl) (splay-to (rope-left rr) direction-finder arg2))
        (values v (replace-both rrl
                                (replace-right r (rope-left rrl))
                                (replace-left rr (rope-right rrl))))]
       ['right ;; zig-zig
        (define-values (v rrr) (splay-to (rope-right rr) direction-finder arg2))
        (values v (replace-left rrr (replace-both rr
                                                  (replace-right r (rope-left rr))
                                                  (and rrr (rope-left rrr)))))])]))

(define (rope-lo r)
  (rope-size (rope-left r)))

(define (rope-lo+hi r)
  (define lo (rope-lo r))
  (match (rope-piece r)
    [(strand _ _ count) (values lo (+ lo count))]
    [_ (values lo lo)]))

(define (find-position pos r)
  (if (rope-empty? r)
      (values 'here (zero? pos))
      (let-values (((lo hi) (rope-lo+hi r)))
        (cond
         [(< pos lo) (values 'left pos)]
         [(< pos hi) (values 'here #t)]
         [else (values 'right (- pos hi))]))))

;; (define (dump-mark-tree r [offset 0])
;;   (define (-> r offset)
;;     (if r
;;         (let-values (((lo hi) (rope-lo+hi r)))
;;           (list ;; (list 'text (strand->string (rope-strand r)))
;;                 (list 'marks* (set->list (rope-marks* r)))
;;                 (list 'mark-index
;;                       (for*/list (((k t) (in-hash (rope-mark-index r)))
;;                                   ((p v) (in-hash t)))
;;                         (list k (+ p offset lo) v)))
;;                 (list 'bounds offset (+ offset lo) (+ offset hi) (+ offset (rope-size r)))
;;                 (list 'left (-> (rope-left r) offset))
;;                 (list 'right (-> (rope-right r) (+ offset hi)))))
;;         'empty))
;;   (local-require racket/pretty)
;;   (pretty-print (-> r offset) (current-error-port))
;;   r)

;; Searches from pos (inclusive) in the direction indicated.
;; Pos points to a mark-position, not a character-position.
(define (find-mark* r forward? mtype start-pos)
  (define (search-here r offset start-pos)
    (match (rope-piece r)
      [(? marks? marks)
       (and (hash-has-key? marks mtype)
            (cons (+ (rope-lo r) offset) (hash-ref marks mtype)))]
      [_ #f]))
  (define (search r offset start-pos)
    (and r
         (set-member? (rope-marks r) mtype)
         (let-values (((lo hi) (rope-lo+hi r)))
           (if forward?
               (or (and (< start-pos lo) (search (rope-left r) offset start-pos))
                   (and (<= start-pos hi) (search-here r offset (- start-pos lo)))
                   (search (rope-right r) (+ offset hi) (- start-pos hi)))
               (or (and (> start-pos hi) (search (rope-right r) (+ offset hi) (- start-pos hi)))
                   (and (>= start-pos lo) (search-here r offset (- start-pos lo)))
                   (search (rope-left r) offset start-pos)))
           )))
  (search r 0 start-pos))

(define (has-mark? r mtype)
  (and r (set-member? (rope-marks r) mtype)))

(define (find-mark r mtype
                   #:forward? [forward? #t]
                   #:position [start-pos (if forward? 0 (rope-size r))])
  (find-mark* r forward? mtype start-pos))

(define (find-mark-pos r mtype
                       #:forward? [forward? #t]
                       #:position [start-pos (if forward? 0 (rope-size r))])
  (cond [(find-mark* r forward? mtype start-pos) => car]
        [else #f]))

(define (find-all-marks/type r mtype)
  (define (walk r offset acc)
    (if (set-member? (rope-marks r) mtype)
        (let-values (((lo hi) (rope-lo+hi r)))
          (let* ((acc (walk (rope-left r) offset acc))
                 (marks (rope-piece r))
                 (acc (if (and (hash? r) (hash-has-key? r mtype))
                          (hash-set acc (+ lo offset) (hash-ref marks mtype))
                          acc)))
            (walk (rope-right r) (+ offset hi) acc)))
        acc))
  (walk r 0 (hash)))

(define (splay-to-pos what r0 pos [extra (lambda () "")])
  (define-values (found? r1) (splay-to r0 find-position pos))
  (when (not found?) (error what "Invalid position ~a~a" pos (extra)))
  r1)

(define (single-mark-rope mtype value)
  (rope (hasheq mtype value) (empty-rope) (empty-rope) 0 (seteq mtype)))

(define (marks-rope? r)
  (marks? (rope-piece r)))

(define (strand-rope? r)
  (strand? (rope-piece r)))

(define (set-mark r0 mtype position value)
  (define-values (l r) (rope-split r0 position))
  (rope-append (rope-append l (single-mark-rope mtype value)) r))

(define (remove-single-mark r mtype)
  (define p (hash-remove (rope-piece r) mtype))
  (if (hash-empty? p)
      (rope-append (rope-left r) (rope-right r))
      (reindex (struct-copy rope r [piece p]))))

(define (clear-mark r0 mtype position)
  (define-values (l r) (rope-split r0 position))
  (rope-append (if (and (marks-rope? l) (eq? (mark-type-stickiness mtype) 'left))
                   (remove-single-mark l mtype)
                   l)
               (if (and (marks-rope? r) (eq? (mark-type-stickiness mtype) 'right))
                   (remove-single-mark r mtype)
                   r)))

(define (replace-mark r0 mtype new-pos new-value)
  (define pos (find-mark-pos r0 mtype))
  (set-mark (if pos (clear-mark r0 mtype pos) r0) mtype new-pos new-value))

(define (clear-all-marks r)
  (and r
       (if (marks? (rope-piece r))
           (rope-append (clear-all-marks (rope-left r))
                        (clear-all-marks (rope-right r)))
           (struct-copy rope r
                        [marks* (seteq)]
                        [left (clear-all-marks (rope-left r))]
                        [right (clear-all-marks (rope-right r))]))))

(define (rope-size r)
  (if r (rope-size* r) 0))

(define (rope-marks r)
  (if r (rope-marks* r) (seteq)))

(define (reindex r)
  (define p (rope-piece r))
  (struct-copy rope r
               [size* (+ (rope-size (rope-left r))
                         (rope-size (rope-right r))
                         (if (strand? p) (strand-count p) 0))]
               [marks* (set-union (rope-marks (rope-left r))
                                  (rope-marks (rope-right r))
                                  (if (marks? p) (list->seteq (hash-keys p)) (seteq)))]))

(define (split-marks p)
  (values (for/hash [((mtype value) (in-hash p)) #:when (eq? (mark-type-stickiness mtype) 'left)]
            (values mtype value))
          (for/hash [((mtype value) (in-hash p)) #:when (eq? (mark-type-stickiness mtype) 'right)]
            (values mtype value))))

;; Causes p2's values to override those of p1, in case of conflict.
(define (merge-marks p1 p2)
  (for/fold [(p p1)] [((mtype value) (in-hash p2))] (hash-set p mtype value)))

(define (rope-split r0 position)
  (match (splay-to-pos 'rope-split r0 position)
    [(? rope-empty?) (values (empty-rope) (empty-rope))]
    [(and r (rope p rl rr size marks))
     ;; We know the position is in the root of r.
     (define-values (lo hi) (rope-lo+hi r))
     (cond
       [(= position lo)
        (let-values (((_l rl) (splay-to rl find-position (rope-size rl))))
          (if (and rl (marks? (rope-piece rl)))
              (let-values (((left-p right-p) (split-marks (rope-piece rl))))
                (values (reindex (struct-copy rope rl [piece left-p]))
                        (rope-append (reindex (rope right-p (empty-rope) (empty-rope) 0 (seteq)))
                                     rr)))
              (values rl (replace-left r (empty-rope)))))]
       [(= position hi)
        ;; This only happens when position is right at the end of r0.
        (when (not (rope-empty? rr))
          (error 'rope-split "Internal error: invariant failure at right: ~v / ~v" position r))
        (if (marks? p)
            (let-values (((left-p right-p) (split-marks p)))
              (values (reindex (struct-copy rope r [piece left-p]))
                      (reindex (rope right-p (empty-rope) (empty-rope) 0 (seteq)))))
            (values r (empty-rope)))]
       [(marks? p)
        (error 'rope-split "Internal error: invariant failure at top: ~v / ~v" position r)]
       [else
        (define offset (- position lo))
        (values (reindex (rope (substrand p 0 offset) rl (empty-rope) 0 (seteq)))
                (reindex (rope (substrand p offset) (empty-rope) rr 0 (seteq))))])]))

(define (rope-append rl0 rr0)
  (cond
   [(rope-empty? rl0) rr0]
   [(rope-empty? rr0) rl0]
   [else
    (define-values (_l rl) (splay-to rl0 find-position (rope-size rl0)))
    (define-values (_r rr) (splay-to rr0 find-position 0))
    ;; Both rl's right and rr's left are (empty-rope).
    (define pl (rope-piece rl))
    (define pr (rope-piece rr))
    (cond
      [(and (marks? pl) (marks? pr))
       (reindex (rope (merge-marks pl pr) (rope-left rl) (rope-right rr) 0 (seteq)))]
      [(and (strand? pl) (strand? pr) (strand-maybe-append pl pr)) =>
       (lambda (t)
         (reindex (rope t (rope-left rl) (rope-right rr) 0 (seteq))))]
      [else
       (replace-right rl rr)])]))

(define (rope-concat rs)
  (foldr rope-append (empty-rope) rs))

(define (subrope r0 [lo0 #f] [hi0 #f])
  (define lo (compute-range-index lo0 0 (rope-size r0)))
  (define hi (compute-range-index hi0 (rope-size r0) (rope-size r0)))
  (define-values (_l mr) (rope-split r0 lo))
  (define-values (m _r) (rope-split mr (- hi lo)))
  m)

(define (rope-generator r #:forward? [forward? #t])
  (if forward?
      (generator ()
        (let outer ((r r))
          (and r
               (begin (outer (rope-left r))
                      (match (rope-piece r)
                        [(strand text offset count)
                         (do ((i 0 (+ i 1)))
                             ((= i count))
                           (yield (string-ref text (+ offset i))))]
                        [(? marks?) (void)])
                      (outer (rope-right r))))))
      (generator ()
        (let outer ((r r))
          (and r
               (begin (outer (rope-right r))
                      (match (rope-piece r)
                        [(strand text offset count)
                         (do ((i (- count 1) (- i 1)))
                             ((negative? i))
                           (yield (string-ref text (+ offset i))))]
                        [(? marks?) (void)])
                      (outer (rope-left r))))))))

(define (open-input-rope r #:name [name "<rope>"])
  (define buffer #"")
  (define offset 0)
  (define stack (list r))
  (define (read-bytes! bs)
    (let retry ()
      (define available (- (bytes-length buffer) offset))
      (if (zero? available)
          (match stack
            ['() eof]
            [(cons (? rope-empty?) rest)
             (set! stack rest)
             (retry)]
            [(cons (? strand? new-strand) rest)
             (set! buffer (string->bytes/utf-8 (strand->string new-strand)))
             (set! offset 0)
             (set! stack rest)
             (retry)]
            [(cons (? rope? r) rest)
             (define p (rope-piece r))
             (set! stack
                   (if (strand? p)
                       (list* (rope-left r) p (rope-right r) stack)
                       (list* (rope-left r)   (rope-right r) stack)))
             (retry)])
          (let ((count (min available (bytes-length bs))))
            (bytes-copy! bs 0 buffer offset (+ offset count))
            (set! offset (+ offset count))
            count))))
  (make-input-port name
                   read-bytes!
                   #f
                   void))

(define (rope-seek r0 pos)
  (splay-to-pos 'rope-seek r0 pos))

;; (require racket/trace)
;; (trace splay-to find-position rope-concat rope-append rope-split rope->string)

(module+ test
  (require (only-in racket/string string-append*))

  (check-equal? (rope-size (empty-rope)) 0)

  (define-syntax-rule (find-mark/values arg ...)
    (match (find-mark arg ...)
      [(cons p v) (values p v)]
      [#f (values #f #f)]))

  (define mtype1 (mark-type "Mark1" 'left))
  (define mtype2 (mark-type "Mark2" 'right))

  (define demo-rope
    (let ((r (lambda (s ms L R)
               (define m
                 (for/fold [(r (string->rope s))] [(e ms)]
                   (match-define (list mtype pos val) e)
                   (set-mark r mtype pos val)))
               (rope-append L (rope-append m R))))
          (m1 (list (list mtype1 0 #t) (list mtype1 1 #t) (list mtype2 3 #t))))
      ;;  a b c d e f g h i j k l m n o p q r s t u
      ;; 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2
      ;; 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      ;;   *   + *   + *   + *   + *   + *   + *   +
      ;; *     *     *     *     *     *     *
      ;;
      (r "jkl"
         m1
         (r "def"
            m1
            (r "abc" m1 (empty-rope) (empty-rope))
            (r "ghi" m1 (empty-rope) (empty-rope)))
         (r "pqr"
            m1
            (r "mno" m1 (empty-rope) (empty-rope))
            (r "stu" m1 (empty-rope) (empty-rope))))))

  (check-equal? (find-all-marks/type demo-rope mtype1)
                (for/hash ((i '(0 1 3 4 6 7 9 10 12 13 15 16 18 19))) (values i #t)))
  (check-equal? (find-all-marks/type demo-rope mtype2)
                (for/hash ((i '(3 6 9 12 15 18 21))) (values i #t)))

  (let ((is '(0 1 3 4 6 7 9 10 12 13 15 16 18 19)))
    (for ((i is))
      (check-equal? (find-all-marks/type (clear-mark demo-rope mtype1 i) mtype1)
                    (for/hash ((j (remove i is))) (values j #t)))))

  (let ((is '(3 6 9 12 15 18 21)))
    (for ((i is))
      (check-equal? (find-all-marks/type (clear-mark demo-rope mtype2 i) mtype2)
                    (for/hash ((j (remove i is))) (values j #t)))))

  (let-values (((l r) (rope-split demo-rope 3)))
    (check-equal? (find-all-marks/type l mtype2) (hash))
    (check-equal? (find-all-marks/type r mtype2)
                  (for/hash ((i '(0 3 6 9 12 15 18))) (values i #t))))

  (let-values (((l r) (rope-split demo-rope 3)))
    (check-equal? (find-all-marks/type l mtype1)
                  (for/hash ((i '(0 1 3))) (values i #t)))
    (check-equal? (find-all-marks/type r mtype1)
                  (for/hash ((i '(1 3 4 6 7 9 10 12 13 15 16))) (values i #t))))

  (define (test-with-pieces string-pieces)
    (define rope-pieces (map string->rope string-pieces))
    (define text (string-append* string-pieces))
    (check-equal? (rope->string (car rope-pieces)) (car string-pieces))
    (check-equal? (rope->string (rope-concat rope-pieces)) text)
    (check-equal? (rope-size (rope-concat rope-pieces)) (string-length text))

    (check-eq? (rope-append (empty-rope) (car rope-pieces)) (car rope-pieces))
    (check-eq? (rope-append (car rope-pieces) (empty-rope)) (car rope-pieces))

    (let loop ((n 1000) (r0 (rope-concat rope-pieces)))
      (when (positive? n)
        (define pos (random (+ (rope-size r0) 1)))
        ;; (pretty-print (list pos r0))
        (define-values (found? r) (splay-to r0 find-position pos))
        (check-true found?)
        (check-equal? (rope->string r) text)
        (loop (- n 1) r)))

    (let*-values (((r) (set-mark (rope-concat rope-pieces) mtype1 9 "original"))
                  ((_) (check-equal? (rope->string r) text))
                  ((pos val) (find-mark/values r mtype1))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "original"))
                  ((r) (clear-mark r mtype1 pos))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash)))
                  ((pos val) (find-mark/values r mtype1))
                  ((_) (check-false pos))
                  ((_) (check-false val))
                  ((r) (set-mark r mtype1 9 "second"))
                  ((pos val) (find-mark/values r mtype1))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "second"))
                  ((r) (set-mark r mtype1 6 "first"))
                  ((r) (set-mark r mtype2 6 "third"))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash 6 "first" 9 "second")))
                  ((_) (check-equal? (find-all-marks/type r mtype2) (hash 6 "third")))
                  ((pos val) (find-mark/values r mtype1 #:forward? #f))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "second"))
                  ((pos val) (find-mark/values r mtype1))
                  ((_) (check-equal? pos 6))
                  ((_) (check-equal? val "first"))
                  ((l r) (rope-split r pos))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash 3 "second")))
                  ((_) (check-equal? (find-all-marks/type l mtype1) (hash 6 "first")))
                  ((_) (check-equal? (find-all-marks/type r mtype2) (hash 0 "third")))
                  ((_) (check-equal? (find-all-marks/type l mtype2) (hash)))
                  ((_) (check-equal? (rope->string l) (substring text 0 6)))
                  ((_) (check-equal? (rope->string r) (substring text 6 (string-length text))))
                  ((_) (check-equal? (rope-marks l) (seteq mtype1)))
                  ((_) (check-equal? (rope-marks r) (seteq mtype1 mtype2)))
                  ((l r) (rope-split r 3))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash)))
                  ((_) (check-equal? (find-all-marks/type l mtype1) (hash 3 "second")))
                  ((_) (check-equal? (find-all-marks/type r mtype2) (hash)))
                  ((_) (check-equal? (find-all-marks/type l mtype2) (hash 0 "third")))
                  ((_) (check-equal? (rope->string l) (substring text 6 9)))
                  ((_) (check-equal? (rope->string r) (substring text 9 (string-length text)))))
      (void)))

  (define prejudice-pieces
    (list "It is a truth universally acknowledged, that a single man in possession of a good fortune must be in want of a wife.\n"
          "\n"
          "However little known the feelings or views of such a man may be on his first entering a neighbourhood, this truth is so well fixed in the minds of the surrounding families, that he is considered as the rightful property of some one or other of their daughters.\n"
          "\n"
          "``My dear Mr. Bennet,'' said his lady to him one day, ``have you heard that Netherfield Park is let at last?''\n"
          "\n"
          "Mr. Bennet replied that he had not.\n"))

  (define (atomize-pieces pieces)
    (map string (string->list (string-append* pieces))))

  (test-with-pieces (list "hello" ", " "world"))
  (test-with-pieces prejudice-pieces)
  (test-with-pieces (atomize-pieces prejudice-pieces))

  (check-equal? (call-with-values (lambda () (rope-split (empty-rope) 0)) list)
                (list (empty-rope) (empty-rope)))

  (check-equal? (map rope->string
                     (call-with-values (lambda () (rope-split (string->rope "abc") 0)) list))
                (list "" "abc"))
  (check-equal? (map rope->string
                     (call-with-values (lambda () (rope-split (string->rope "abc") 2)) list))
                (list "ab" "c"))
  (check-equal? (map rope->string
                     (call-with-values (lambda () (rope-split (string->rope "abc") 3)) list))
                (list "abc" ""))

  (check-equal? (map (lambda (i) (compute-range-index i 'default 10))
                     (list 0 10 3 -1 -2 11 12 -8 -9 -10 -11 -12))
                (list      0 10 3  9  8 10 10  2  1   0   0   0))

  (let* ((r (rope-append (string->rope (make-string 10 #\a))
                         (string->rope (make-string (* 2 *glom-up-to*) #\z))))
         (_ (check-equal? (rope-size r) (+ 10 (* 2 *glom-up-to*))))
         (r (set-mark r mtype1 (rope-size r) #t))
         (r (splay-to-pos 'testing r 0))
         (pos (find-mark-pos r mtype1)))
    (check-equal? pos 266))

  (let*-values (((r) (string->rope "hello"))
                ((r) (set-mark r mtype2 (rope-size r) #t))
                ((l r) (rope-split r (find-mark-pos r mtype2)))
                ((_) (check-equal? (rope->string l) "hello"))
                ((_) (check-equal? (rope->string r) ""))
                ((_) (check-equal? (rope-marks l) (seteq)))
                ((_) (check-equal? (rope-marks r) (seteq mtype2))))
    (void))

  (let*-values (((xs) (make-string 128 #\x))
                ((r) (string->rope (string-append "hello " xs)))
                ((r) (set-mark r mtype2 3 #t))
                ((l mr) (rope-split r (find-mark-pos r mtype2)))
                ((m r) (rope-split mr 1))
                ((_) (check-equal? (rope->string l) "hel"))
                ((_) (check-equal? (rope->string m) "l"))
                ((_) (check-equal? (rope->string r) (string-append "o " xs)))
                ((_) (check-equal? (rope-marks l) (seteq)))
                ((_) (check-equal? (rope-marks m) (seteq mtype2)))
                ((_) (check-equal? (rope-marks r) (seteq)))
                ((new-m) (set-mark (empty-rope) mtype2 0 #t))
                ((r) (rope-append (rope-append l new-m) r))
                ((_) (check-equal? (rope->string r) (string-append "helo " xs)))
                ((_) (check-equal? (find-mark-pos r mtype2) 3))
                ((r) (clear-mark r mtype2 (find-mark-pos r mtype2)))
                ((_) (check-equal? (find-mark-pos r mtype2) #f)))
    (void))

  (check-equal? (read (open-input-rope (string->rope "(a b c)"))) '(a b c))
  (check-true (eof-object? (read (open-input-rope (empty-rope)))))
  )
