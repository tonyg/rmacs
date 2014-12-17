#lang racket/base
;; Ropes for text editing

(provide empty-strand
         string->strand
         strand->string
         substrand
         strand-equal?
         strand-empty?

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

         has-mark?
         lookup-mark
         set-mark
         clear-mark
         update-mark)

(require racket/set)
(require racket/match)

(module+ test (require rackunit racket/pretty))

;; A Mark is a Symbol.

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
              marks* ;; Set of Mark
              mark-index ;; Hashtable from Mark to Number, marks in this span
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

(define (substrand t0 [lo0 #f] [hi0 #f])
  (define t (if (string? t0) (string->strand t0) t0))
  (define lo (if (not lo0)
                 0
                 (modulo lo0 (strand-count t))))
  (define hi (if (not hi0)
                 (strand-count t)
                 (modulo hi0 (strand-count t))))
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

(define (strand->rope t)
  (rope t #f #f (strand-count t) (set) (hash)))

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
  (if (not r)
      (values 'here (zero? pos))
      (let-values (((lo hi) (rope-lo+hi r)))
        (cond
         [(< pos lo) (values 'left pos)]
         [(< pos hi) (values 'here #t)]
         [else (values 'right (- pos hi))]))))

(define (find-mark mark)
  (define (walk offset r)
    (cond
     [(not r)
      (values 'here #f)]
     [(hash-ref (rope-mark-index r) mark #f) =>
      (lambda (p) (values 'here (+ offset (rope-lo r) p)))]
     [(set-member? (rope-marks (rope-left r)) mark)
      (values 'left offset)]
     [(set-member? (rope-marks (rope-right r)) mark)
      (values 'right (+ offset (rope-lo r) (strand-count (rope-strand r))))]
     [else
      (values 'here #f)]))
  ;; (trace walk)
  walk)

(define (has-mark? r mark)
  (and r (set-member? (rope-marks r) mark)))

(define (lookup-mark r mark)
  (splay-to r (find-mark mark) 0))

(define (set-mark r0 mark position)
  (when (set-member? (rope-marks r0) mark) (error 'set-mark "Duplicate mark: ~a" mark))
  (define-values (found? r1) (splay-to r0 find-position position))
  (when (not found?) (error 'set-mark "Invalid position ~a setting mark ~a" position mark))
  (reindex
   (if (not r1)
       (rope (empty-strand) #f #f 'will-be-recomputed 'will-be-recomputed (hash mark position))
       (struct-copy rope r1
                    [mark-index (hash-set (rope-mark-index r1) mark (- position (rope-lo r1)))]))))

(define (clear-mark r0 mark)
  (define-values (old-pos r) (lookup-mark r0 mark))
  (if old-pos
      (struct-copy rope r
                   [marks* (set-remove (rope-marks* r) mark)]
                   [mark-index (hash-remove (rope-mark-index r) mark)])
      r))

(define (update-mark r0 mark position)
  (set-mark (if (has-mark? r0 mark)
                (clear-mark r0 mark)
                r0)
            mark
            position))

(define (rope-size r)
  (if r (rope-size* r) 0))

(define (rope-marks r)
  (if r (rope-marks* r) (set)))

(define (reindex r)
  (struct-copy rope r
               [size* (+ (rope-size (rope-left r))
                         (rope-size (rope-right r))
                         (strand-count (rope-strand r)))]
               [marks* (set-union (rope-marks (rope-left r))
                                  (rope-marks (rope-right r))
                                  (list->set (hash-keys (rope-mark-index r))))]))

(define (rope-split r0 position)
  (define-values (found? r) (splay-to r0 find-position position))
  (when (not found?) (error 'rope-split "Invalid position ~a" position))
  ;; We know the position is in the root of r.
  (match-define (rope t rl rr size marks mark-index) r)
  (define-values (lo hi) (rope-lo+hi r))
  (define offset (- position lo))
  (define-values (left-index marks-at-split right-index)
    (partition-mark-index mark-index offset))
  (define left-strand (substrand t 0 offset))
  (define right-strand (substrand t offset))
  (values (if (strand-empty? left-strand)
              rl
              (reindex (rope left-strand rl #f 'will-be-recomputed (set) left-index)))
          marks-at-split
          (if (strand-empty? right-strand)
              rr
              (reindex (rope right-strand #f rr 'will-be-recomputed (set) right-index)))))

(define (partition-mark-index index offset)
  (for/fold [(l (hash)) (m (set)) (r (hash))]
            [((mark pos) (in-hash index))]
    (values (if (< pos offset) (hash-set l mark pos) l)
            (if (= pos offset) (set-add m mark) m)
            (if (> pos offset) (hash-set r mark (- pos offset)) r))))

(define (rope-append rl0 rr0)
  (cond
   [(not rl0) rr0]
   [(not rr0) rl0]
   [else
    (define-values (_l rl) (splay-to rl0 find-position (rope-size rl0)))
    (define-values (_r rr) (splay-to rr0 find-position 0))
    ;; Both rl's right and rr's left are #f.
    (define t (strand-maybe-append (rope-strand rl) (rope-strand rr)))
    (if t
        (let ((merged-index (merge-mark-indexes (rope-mark-index rl)
                                                (rope-mark-index rr)
                                                (strand-count (rope-strand rl)))))
          (reindex (rope t (rope-left rl) (rope-right rr) 'will-be-recomputed (set) merged-index)))
        (replace-right rl rr))]))

(define (rope-concat rs)
  (foldr rope-append (empty-rope) rs))

(define (merge-mark-indexes li ri offset)
  (for/fold [(i li)] [((k v) (in-hash ri))]
    (if (hash-has-key? i k)
        (error 'merge-mark-indexes "Duplicate mark: ~a" k)
        (hash-set i k (+ offset v)))))

(define (subrope* r0 [lo0 #f] [hi0 #f])
  (define lo (if (not lo0)
                 0
                 (modulo lo0 (rope-size r0))))
  (define hi (if (not hi0)
                 (rope-size r0)
                 (modulo hi0 (rope-size r0))))
  (define-values (_l left-marks-at-split mr) (rope-split r0 lo))
  (define-values (m right-marks-at-split _r) (rope-split mr (- hi lo)))
  (values left-marks-at-split m right-marks-at-split))

(define (subrope r0 [lo0 #f] [hi0 #f])
  (define-values (_l m _r) (subrope* r0 lo0 hi0))
  m)

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

    (let*-values (((r) (set-mark (rope-concat rope-pieces) 'mark 9))
                  ((_) (check-equal? (rope->string r) text))
                  ((pos r) (lookup-mark r 'mark))
                  ((_) (check-equal? pos 9))
                  ((r) (clear-mark r 'mark))
                  ((pos r) (lookup-mark r 'mark))
                  ((_) (check-false pos))
                  ((r) (update-mark r 'mark 9))
                  ((pos r) (lookup-mark r 'mark))
                  ((_) (check-equal? pos 9))
                  ((r) (update-mark r 'mark 6))
                  ((pos r) (lookup-mark r 'mark))
                  ((_) (check-equal? pos 6))
                  ((l ms r) (rope-split r pos))
                  ((_) (check-equal? ms (set 'mark)))
                  ((_) (check-equal? (rope->string l) (substring text 0 6)))
                  ((_) (check-equal? (rope->string r) (substring text 6 (string-length text))))
                  ((l ms r) (rope-split r 3))
                  ((_) (check-equal? ms (set)))
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
  )
