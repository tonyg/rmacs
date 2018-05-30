#lang racket/base
;; Ropes for text, attributes, etc

(provide (rename-out [rope?* rope?])
         rope-empty?
         rope-empty
         piece->rope

         rope-piece
         update-piece

         rope-size
         rope-index
         rope-seek
         rope-split
         rope-append
         rope-concat
         subrope

         find-in-index
         find-pos-in-index
         find-all-in-index
         rope-map
         rope-map/key

         in-rope
         rope->piece-list
         rope->searchable-string
         rope->searchable-generator
         rope->searchable-port
         )

(require racket/stream)
(require (only-in racket/string string-append*))
(require racket/match)

(require "rope/piece.rkt")
(require "rope/index.rkt")
(require "rope/range.rkt")
(require "rope/string.rkt")

;; A Rope is a splay tree representing a long piece of text.
;; #f is the empty Rope; otherwise a (rope) struct instance.
(struct rope (piece ;; Piece
              left ;; Rope (possibly empty)
              right ;; Rope (possibly empty)
              size* ;; Number, total length of this rope
              index* ;; Index
              ) #:prefab)

(define (rope-empty? r) (eq? r #f))

(define-match-expander rope-empty
  (syntax-rules () [(_) #f])
  (syntax-rules () [(_) #f]))

(define (rope?* r)
  (or (rope-empty? r)
      (rope? r)))

(define (make-rope p l r)
  (if (piece-empty? p)
      (rope-append l r)
      (reindex (rope p l r (piece-size p) #f))))

(define (piece->rope p)
  (make-rope p (rope-empty) (rope-empty)))

(define (rope-size r)
  (match r
    [(rope-empty) 0]
    [_ (rope-size* r)]))

(define (rope-lo r)
  (rope-size (rope-left r)))

(define (rope-lo+hi r)
  (define lo (rope-lo r))
  (values lo (+ lo (piece-size (rope-piece r)))))

(define (find-position pos r)
  (if (rope-empty? r)
      (values 'here (zero? pos))
      (let-values (((lo hi) (rope-lo+hi r)))
        (cond
         [(< pos lo) (values 'left pos)]
         [(< pos hi) (values 'here #t)]
         [else (values 'right (- pos hi))]))))

(define (rope-index r)
  (match r
    [(rope-empty) #f]
    [_ (rope-index* r)]))

(define (reindex r)
  (match-define (rope p rl rr _ _) r)
  (struct-copy rope r
    [size* (+ (rope-size rl) (rope-size rr) (piece-size p))]
    [index* (index-merge (index-merge (piece->index p) (rope-index rl)) (rope-index rr))]))

(define (replace-left r n) (if (rope-empty? r) n (reindex (struct-copy rope r [left n]))))
(define (replace-right r n) (if (rope-empty? r) n (reindex (struct-copy rope r [right n]))))
(define (replace-both r rl rr) (reindex (struct-copy rope r [left rl] [right rr])))

(define (splay-to what r pos0)
  (define (ensure-found ok? r)
    (if (not ok?)
        (error what "Invalid position ~a" pos0)
        r))
  (let walk ((r r) (pos0 pos0))
    ;; zig: last. desired position is a direct (left/right) child of r.
    ;; zig-zig: desired position is within a (left-left/right-right) grandchild of r.
    ;; zig-zag: desired position is within a (left-right/right-left) grandchild of r.
    (match/values (find-position pos0 r)
      [('here ok?) (ensure-found ok? r)]
      [('left pos1)
       (define rl (rope-left r))
       (match/values (find-position pos1 rl)
         [('here _) ;; zig
          (replace-right rl (replace-left r (and rl (rope-right rl))))]
         [('left pos2) ;; zig-zig
          (define rll (walk (rope-left rl) pos2))
          (replace-right rll (replace-both rl
                                           (and rll (rope-right rll))
                                           (replace-left r (rope-right rl))))]
         [('right pos2) ;; zig-zag
          (define rlr (walk (rope-right rl) pos2))
          (replace-both rlr
                        (replace-right rl (rope-left rlr))
                        (replace-left r (rope-right rlr)))])]
      [('right pos1)
       (define rr (rope-right r))
       (match/values (find-position pos1 rr)
         [('here _) ;; zig
          (replace-left rr (replace-right r (and rr (rope-left rr))))]
         [('left pos2) ;; zig-zag
          (define rrl (walk (rope-left rr) pos2))
          (replace-both rrl
                        (replace-right r (rope-left rrl))
                        (replace-left rr (rope-right rrl)))]
         [('right pos2) ;; zig-zig
          (define rrr (walk (rope-right rr) pos2))
          (replace-left rrr (replace-both rr
                                          (replace-right r (rope-left rr))
                                          (and rrr (rope-left rrr))))])])))

(define (rope-seek r0 pos)
  (define r (splay-to 'rope-seek r0 pos))
  (if (zero? pos)
      ;; Needed because splaying all the way left, when there is a
      ;; zero-sized piece at the very left, leaves us with that piece in the
      ;; left branch.
      (let hoist-left-piece ((r r))
        (match r
          [(rope _ (? rope? rl) _ _ _)
           (hoist-left-piece (replace-right rl (replace-left r (rope-right rl))))]
          [_ r]))
      r))

(define (rope-split r0 position)
  (match (splay-to 'rope-split r0 position)
    [(rope-empty) (values (rope-empty) (rope-empty))]
    [(and r (rope p rl rr _ _))
     ;; We know the position is in the root of r.
     (define-values (lo hi) (rope-lo+hi r))
     (cond
       [(= position hi)
        ;; This only happens when position is right at the end of r0.
        ;; We check this condition *first* because (= position lo)
        ;; might also be true, in the case where (zero? (piece-size p)).
        (when (not (rope-empty? rr))
          (error 'rope-split "Internal error: invariant failure at right: ~v / ~v" position r))
        (define-values (left-p right-p) (piece-split p (- position lo)))
        (values (update-piece r left-p)
                (piece->rope right-p))]
       [(= position lo)
        (if (rope-empty? rl)
            (values rl r)
            (let* ((rl (splay-to 'rope-split rl (rope-size rl)))
                   (rlp (rope-piece rl)))
              (define-values (left-p right-p) (piece-split rlp (piece-size rlp)))
              (values (update-piece rl left-p)
                      (rope-append (piece->rope right-p) (replace-left r (rope-empty))))))]
       [else
        (define-values (left-p right-p) (piece-split p (- position lo)))
        (values (rope-append rl (piece->rope left-p) 'left)
                (rope-append (piece->rope right-p) rr))])]))

(define (rope-append rl0 rr0 [bias 'right])
  (cond [(rope-empty? rl0) rr0]
        [(rope-empty? rr0) rl0]
        [else
         (define rl (rope-seek rl0 (rope-size rl0)))
         (define rr (rope-seek rr0 0))
         ;; Both rl's right and rr's left are (rope-empty).
         ;; Both rl's and rr's pieces are non `piece-empty?`.
         (piece-merge (rope-piece rl)
                      (rope-piece rr)
                      (lambda (p) (make-rope p (rope-left rl) (rope-right rr)))
                      (lambda ()
                        (match bias
                          ['right (replace-right rl rr)]
                          ['left (replace-left rr rl)])))]))

(define (rope-concat rs)
  (foldr rope-append (rope-empty) rs))

(define (subrope r0 [lo0 #f] [hi0 #f])
  (define-values (lo hi) (compute-range-lo+hi lo0 hi0 (rope-size r0)))
  (define-values (_l mr) (rope-split r0 lo))
  (define-values (m _r) (rope-split mr (- hi lo)))
  m)

;; Searches from pos (inclusive) in the direction indicated.
(define (find-in-index* r forward? key start-pos)
  (define (search-here r offset start-pos)
    (match (piece-match (rope-piece r) forward? key start-pos)
      ['() #f]
      [(cons (cons pos value) _) (cons (+ offset (rope-lo r) pos) value)]))
  (define (search r offset start-pos)
    (and (not (rope-empty? r))
         (index-contains? (rope-index r) key)
         (let-values (((lo hi) (rope-lo+hi r)))
           (if forward?
               (or (and (<= start-pos lo) (search (rope-left r) offset start-pos))
                   (and (<= start-pos hi) (search-here r offset (- start-pos lo)))
                   (search (rope-right r) (+ offset hi) (- start-pos hi)))
               (or (and (>= start-pos hi) (search (rope-right r) (+ offset hi) (- start-pos hi)))
                   (and (>= start-pos lo) (search-here r offset (- start-pos lo)))
                   (search (rope-left r) offset start-pos))))))
  (search r 0 start-pos))

(define (find-in-index r key
                       #:forward? [forward? #t]
                       #:position [start-pos (if forward? 0 (rope-size r))])
  (find-in-index* r forward? key start-pos))

(define (find-pos-in-index r key
                           #:forward? [forward? #t]
                           #:position [start-pos (if forward? 0 (rope-size r))])
  (cond [(find-in-index* r forward? key start-pos) => car]
        [else #f]))

(define (find-all-in-index r key)
  (let walk ((r r) (offset 0) (acc (hash)))
    (if (or (rope-empty? r) (not (index-contains? (rope-index r) key)))
        acc
        (let-values (((lo hi) (rope-lo+hi r)))
          (let* ((acc (walk (rope-left r) offset acc))
                 (p (rope-piece r))
                 (acc (for/fold [(acc acc)] [(e (in-list (piece-match p #t key 0)))]
                        (match-define (cons pos value) e)
                        (hash-set acc (+ offset lo pos) value))))
            (walk (rope-right r) (+ offset hi) acc))))))

(define (update-piece r p)
  (make-rope p (rope-left r) (rope-right r)))

(define (rope-map r f)
  (let walk ((r r))
    (if (rope-empty? r)
        r
        (rope-append (walk (rope-left r))
                     (rope-append (piece->rope (f (rope-piece r)))
                                  (walk (rope-right r)))))))

(define (rope-map/key r key f)
  (let walk ((r r))
    (if (or (rope-empty? r) (not (index-contains? (rope-index r) key)))
        r
        (rope-append (walk (rope-left r))
                     (rope-append (piece->rope (f (rope-piece r)))
                                  (walk (rope-right r)))))))

(define (in-rope r #:forward? [forward? #t])
  (if forward?
      (let loop ((r r))
        (if (rope-empty? r)
            empty-stream
            (let ((r (rope-seek r 0)))
              (stream-cons (rope-piece r) (loop (rope-right r))))))
      (let loop ((r r))
        (if (rope-empty? r)
            empty-stream
            (let ((r (rope-seek r (rope-size r))))
              (stream-cons (rope-piece r) (loop (rope-left r))))))))

(define (rope->piece-list r)
  (let walk ((r r) (tail '()))
    (match r
      [(rope-empty) tail]
      [(rope p rl rr _ _) (walk rl (cons p (walk rr tail)))])))

(define (rope->searchable-string r)
  (string-append* (map piece->searchable-string (rope->piece-list r))))

(define (rope->searchable-generator r #:forward? [forward? #t])
  (define stack '())
  (define text "")
  (define offset 0)
  (define count 0)
  (define (push! x) (when x (set! stack (cons x stack))))
  (define (pop!) (and (pair? stack) (begin0 (car stack) (set! stack (cdr stack)))))
  (if forward?
      (let ()
        (define (push-r! r)
          (when r
            (push! (rope-right r))
            (push! (rope-piece r))
            (push-r! (rope-left r))))

        (push-r! r)
        (define i 0)
        (define (g)
          (if (< i count)
              (begin0 (string-ref text (+ offset i))
                (set! i (+ i 1)))
              (match (pop!)
                [#f #f]
                [(? rope? r)
                 (push-r! r)
                 (g)]
                [(strand s o c)
                 (set! text s)
                 (set! offset o)
                 (set! count c)
                 (set! i 0)
                 (g)]
                [p
                 (set! text (piece->searchable-string p))
                 (set! offset 0)
                 (set! count (string-length text))
                 (set! i 0)
                 (g)])))
        g)
      (let ()
        (define (push-r! r)
          (when r
            (push! (rope-left r))
            (push! (rope-piece r))
            (push-r! (rope-right r))))

        (push-r! r)
        (define i (- count 1))
        (define (g)
          (if (>= i 0)
              (begin0 (string-ref text (+ offset i))
                (set! i (- i 1)))
              (match (pop!)
                [#f #f]
                [(? rope? r)
                 (push-r! r)
                 (g)]
                [(strand s o c)
                 (set! text s)
                 (set! offset o)
                 (set! count c)
                 (set! i (- count 1))
                 (g)]
                [p
                 (set! text (piece->searchable-string p))
                 (set! offset 0)
                 (set! count (string-length text))
                 (set! i (- count 1))
                 (g)])))
        g)))

(define (rope->searchable-port r #:forward? [forward? #t] #:name [name "<rope>"])
  (define buffer #"")
  (define offset 0)
  (define stack (list r))
  (define (read-bytes! bs)
    (let retry ()
      (define available (- (bytes-length buffer) offset))
      (if (zero? available)
          (match stack
            ['() eof]
            [(cons (rope-empty) rest)
             (set! stack rest)
             (retry)]
            [(cons (? rope? r) rest)
             (set! stack
                   (if forward?
                       (list* (rope-left r) (rope-piece r) (rope-right r) rest)
                       (list* (rope-right r) (rope-piece r) (rope-left r) rest)))
             (retry)]
            [(cons p rest) ;; p is a `piece?`, by elimination
             (define str (piece->searchable-string p))
             (set! buffer (string->bytes/utf-8 (if forward? str (naive-string-reverse str))))
             (set! offset 0)
             (set! stack rest)
             (retry)])
          (let ((count (min available (bytes-length bs))))
            (bytes-copy! bs 0 buffer offset (+ offset count))
            (set! offset (+ offset count))
            count))))
  (make-input-port name
                   read-bytes!
                   #f
                   void))

(module+ for-test
  (provide splay-to))
