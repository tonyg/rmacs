#lang racket/base
;; Ropes for text editing

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
         rope-seek

         (struct-out mark-type)

         has-mark?
         find-mark
         find-mark-pos
         find-all-marks/type
         set-mark
         clear-mark
         replace-mark
         clear-all-marks)

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

;; A MarkType is a (mark-type String Stickiness). MarkTypes can be
;; associated with a set of Any values at each position in the rope.
(struct mark-type (name stickiness) #:transparent)

;; A Strand is a (strand String Number Number), representing a
;; substring of a string.
(struct strand (text offset count) #:transparent)

;; A Rope is a splay tree representing a long piece of text.
;; #f is the empty Rope; otherwise a (rope) struct instance.
;; INVARIANT: Adjacent ropes will be merged to maximize sharing.
(struct rope (strand ;; Strand
              left ;; Rope or #f
              right ;; Rope or #f
              size* ;; Number, total length of this rope
              marks* ;; (Seteq MarkType)
              mark-index ;; (Hasheq MarkType (Hash Number (Set Any))), marks in this span
              ) #:transparent)

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

(define (compute-index index default limit)
  (cond [(not index) default]
        [(zero? limit) 0]
        [else (modulo index limit)]))

(define (substrand t0 [lo0 #f] [hi0 #f])
  (define t (if (string? t0) (string->strand t0) t0))
  (define lo (compute-index lo0 0 (strand-count t)))
  (define hi (compute-index hi0 (strand-count t) (strand-count t)))
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
  (zero? (strand-count t)))

(module+ test
  (check-equal? (strand-count (empty-strand)) 0)
  (check-equal? (strand-count (string->strand "")) 0)
  (check-true (strand-equal? (empty-strand) (string->strand ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ropes

(define (empty-rope) #f)

(define (rope-empty? r)
  (equal? r (empty-rope)))

(define (rope?* r)
  (or (rope-empty? r)
      (rope? r)))

(define (strand->rope t)
  (rope t (empty-rope) (empty-rope) (strand-count t) (seteq) (hasheq)))

(define (string->rope s)
  (strand->rope (string->strand s)))

(define (rope->string r)
  (define buf (make-string (rope-size r)))
  (let fill! ((r r) (offset 0))
    (when r
      (fill! (rope-left r) offset)
      (define lo (rope-lo r))
      (define s (rope-strand r))
      (string-copy! buf
                    (+ offset lo)
                    (strand-text s)
                    (strand-offset s)
                    (+ (strand-offset s) (strand-count s)))
      (fill! (rope-right r) (+ offset lo (strand-count s)))))
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
  (values lo (+ lo (strand-count (rope-strand r)))))

(define (find-position pos r)
  (if (rope-empty? r)
      (values 'here (zero? pos))
      (let-values (((lo hi) (rope-lo+hi r)))
        (cond
         [(< pos lo) (values 'left pos)]
         [(< pos hi) (values 'here #t)]
         [else (values 'right (- pos hi))]))))

;; Searches from pos (inclusive) in the direction indicated.
;; Pos points to a mark-position, not a character-position.
(define (find-mark* r forward? mtype start-pos)
  (define (search-here r start-pos)
    (define marks (hash-ref (rope-mark-index r) mtype #f))
    (define lo (rope-lo r))
    (if (not marks)
        #f
        (let ((pos-comparer (if forward? < >))
              (boundary-comparer (if forward? >= <=)))
          (for/fold [(candidate #f)] [((pos value) (in-hash marks))]
            (if (and (or (not candidate)
                         (pos-comparer pos (car candidate)))
                     (boundary-comparer pos start-pos))
                (cons (+ pos lo) value)
                candidate)))))
  (define (search r start-pos)
    (and r
         (set-member? (rope-marks r) mtype)
         (let-values (((lo hi) (rope-lo+hi r)))
           (cond
            [(< start-pos lo)
             (or (search (rope-left r) start-pos)
                 (and forward? (search-here r (- start-pos lo))))]
            [(> start-pos hi)
             (or (search (rope-right r) (- start-pos hi))
                 (and (not forward?) (search-here r (- start-pos lo))))]
            [else
             (search-here r (- start-pos lo))]))))
  (search r start-pos))

(define (has-mark? r mtype)
  (and r (set-member? (rope-marks r) mtype)))

(define (find-mark r mtype
                   #:forward? [forward? #t]
                   #:position [start-pos (if forward? 0 (rope-size r))])
  (define maybe-pos+val (find-mark* r forward? mtype start-pos))
  (if maybe-pos+val
      (values (car maybe-pos+val) (cdr maybe-pos+val))
      (values #f #f)))

(define (find-mark-pos r mtype
                       #:forward? [forward? #t]
                       #:position [start-pos (if forward? 0 (rope-size r))])
  (cond [(find-mark* r forward? mtype start-pos) => car]
        [else #f]))

(define (mark-union h1 h2 offset)
  (for/fold [(h h1)] [((pos val) (in-hash h2))] (hash-set h (+ offset pos) val)))

(define (find-all-marks/type r mtype)
  (define (walk r)
    (if (set-member? (rope-marks r) mtype)
        (let-values (((lo hi) (rope-lo+hi r)))
          (mark-union (walk (rope-left r))
                      (mark-union (hash-ref (rope-mark-index r) mtype (lambda () (hash)))
                                  (walk (rope-right r))
                                  hi)
                      lo))
        (hash)))
  (walk r))

(define (splay-to-pos what r0 pos [extra (lambda () "")])
  (define-values (found? r1) (splay-to r0 find-position pos))
  (when (not found?) (error what "Invalid position ~a~a" pos (extra)))
  r1)

(define (add-mark-to-table old-marks mtype pos value)
  (define old-mark (hash-ref old-marks mtype (lambda () (hash))))
  (hash-set old-marks mtype (hash-set old-mark pos value)))

(define (set-mark r0 mtype position value)
  (define r (splay-to-pos 'set-mark r0 position (lambda () (format " setting mark ~a" mtype))))
  (reindex
   (if (rope-empty? r)
       (rope (empty-strand)
             (empty-rope)
             (empty-rope)
             'will-be-recomputed
             'will-be-recomputed
             (hasheq mtype (hash position value)))
       (struct-copy rope r [mark-index (add-mark-to-table (rope-mark-index r)
                                                          mtype
                                                          (- position (rope-lo r))
                                                          value)]))))

(define (clear-mark r0 mtype position)
  (define r (splay-to-pos 'clear-mark r0 position (lambda () (format " clearing mark ~a" mtype))))
  (and r
       (reindex
        (struct-copy rope r
                     [mark-index
                      (let* ((old-marks (rope-mark-index r))
                             (old-mark (hash-ref old-marks mtype (lambda () (hash)))))
                        (define new-mark (hash-remove old-mark position))
                        (if (hash-empty? new-mark)
                            (hash-remove old-marks mtype)
                            (hash-set old-marks mtype new-mark)))]))))

(define (replace-mark r0 mtype new-pos new-value)
  (define pos (find-mark-pos r0 mtype))
  (when (not pos) (error 'replace-mark "Mark ~a not found" mtype))
  (set-mark (clear-mark r0 mtype pos) mtype new-pos new-value))

(define (clear-all-marks r)
  (and r
       (struct-copy rope r
                    [marks* (seteq)]
                    [mark-index (hasheq)]
                    [left (clear-all-marks (rope-left r))]
                    [right (clear-all-marks (rope-right r))])))

(define (rope-size r)
  (if r (rope-size* r) 0))

(define (rope-marks r)
  (if r (rope-marks* r) (seteq)))

(define (reindex r)
  (struct-copy rope r
               [size* (+ (rope-size (rope-left r))
                         (rope-size (rope-right r))
                         (strand-count (rope-strand r)))]
               [marks* (set-union (rope-marks (rope-left r))
                                  (rope-marks (rope-right r))
                                  (list->seteq (hash-keys (rope-mark-index r))))]))

(define (rope-split r0 position)
  (match (splay-to-pos 'rope-split r0 position)
    [(? rope-empty?) (values (empty-rope) (empty-rope))]
    [(and r (rope t rl rr size marks mark-index))
     ;; We know the position is in the root of r.
     (define-values (lo hi) (rope-lo+hi r))
     (define offset (- position lo))
     (define-values (left-index right-index) (partition-mark-index mark-index offset))
     (define left-strand (substrand t 0 offset))
     (define right-strand (substrand t offset))
     (values (if (strand-empty? left-strand)
                 rl
                 (reindex
                  (rope left-strand rl (empty-rope) 'will-be-recomputed (seteq) left-index)))
             (if (strand-empty? right-strand)
                 rr
                 (reindex
                  (rope right-strand (empty-rope) rr 'will-be-recomputed (seteq) right-index))))]))

(define (partition-mark-index index offset)
  (for*/fold [(l (hasheq)) (r (hasheq))]
             [((mtype posvals) (in-hash index))
              ((pos val) (in-hash posvals))]
    (values (if (or (< pos offset) (and (= pos offset) (eq? (mark-type-stickiness mtype) 'left)))
                (add-mark-to-table l mtype pos val)
                l)
            (if (or (> pos offset) (and (= pos offset) (eq? (mark-type-stickiness mtype) 'right)))
                (add-mark-to-table r mtype (- pos offset) val)
                r))))

(define (rope-append rl0 rr0)
  (cond
   [(rope-empty? rl0) rr0]
   [(rope-empty? rr0) rl0]
   [else
    (define-values (_l rl) (splay-to rl0 find-position (rope-size rl0)))
    (define-values (_r rr) (splay-to rr0 find-position 0))
    ;; Both rl's right and rr's left are (empty-rope).
    (define t (strand-maybe-append (rope-strand rl) (rope-strand rr)))
    (if t
        (let ((merged-index (merge-mark-indexes (rope-mark-index rl)
                                                (rope-mark-index rr)
                                                (strand-count (rope-strand rl)))))
          (reindex (rope t (rope-left rl) (rope-right rr) 'will-be-recomputed (seteq) merged-index)))
        (replace-right rl rr))]))

(define (rope-concat rs)
  (foldr rope-append (empty-rope) rs))

(define (merge-mark-indexes li ri offset)
  (for*/fold [(i li)]
             [((mtype posvals) (in-hash ri))
              ((pos val) (in-hash posvals))]
    (add-mark-to-table i mtype (+ pos offset) val)))

(define (subrope r0 [lo0 #f] [hi0 #f])
  (define lo (compute-index lo0 0 (rope-size r0)))
  (define hi (compute-index hi0 (rope-size r0) (rope-size r0)))
  (define-values (_l mr) (rope-split r0 lo))
  (define-values (m _r) (rope-split mr (- hi lo)))
  m)

(define (rope-generator r #:forward? [forward? #t])
  (if forward?
      (generator ()
        (let outer ((r r))
          (and r
               (begin (outer (rope-left r))
                      (match-let (((strand text offset count) (rope-strand r)))
                        (do ((i 0 (+ i 1)))
                            ((= i count))
                          (yield (string-ref text (+ offset i)))))
                      (outer (rope-right r))))))
      (generator ()
        (let outer ((r r))
          (and r
               (begin (outer (rope-right r))
                      (match-let (((strand text offset count) (rope-strand r)))
                        (do ((i (- count 1) (- i 1)))
                            ((negative? i))
                          (yield (string-ref text (+ offset i)))))
                      (outer (rope-left r))))))))

(define (rope-seek r0 pos)
  (splay-to-pos 'rope-seek r0 pos))

;; (require racket/trace)
;; (trace splay-to find-position rope-concat rope-append rope-split rope->string)

(module+ test
  (require (only-in racket/string string-append*))

  (check-equal? (rope-size (empty-rope)) 0)

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

    (define mtype1 (mark-type "Mark1" 'left))
    (define mtype2 (mark-type "Mark2" 'right))

    (let*-values (((r) (set-mark (rope-concat rope-pieces) mtype1 9 "original"))
                  ((_) (check-equal? (rope->string r) text))
                  ((pos val) (find-mark r mtype1))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "original"))
                  ((r) (clear-mark r mtype1 pos))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash)))
                  ((pos val) (find-mark r mtype1))
                  ((_) (check-false pos))
                  ((_) (check-false val))
                  ((r) (set-mark r mtype1 9 "second"))
                  ((pos val) (find-mark r mtype1))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "second"))
                  ((r) (set-mark r mtype1 6 "first"))
                  ((r) (set-mark r mtype2 6 "third"))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash 6 "first" 9 "second")))
                  ((_) (check-equal? (find-all-marks/type r mtype2) (hash 6 "third")))
                  ((pos val) (find-mark r mtype1 #:forward? #f))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "second"))
                  ((pos val) (find-mark r mtype1))
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
  )
