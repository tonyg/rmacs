#lang racket

(module+ test
  (require rackunit)

  (require "../rope.rkt")
  (require (submod "../rope.rkt" for-test))
  (require "../mark.rkt")
  (require (submod "../mark.rkt" for-test))
  (require "piece.rkt")
  (require "range.rkt")

  (check-equal? (rope-size (rope-empty)) 0)

  (define-syntax-rule (find-in-index/values arg ...)
    (match (find-in-index arg ...)
      [(cons p v) (values p v)]
      [#f (values #f #f)]))

  (define mtype1 (mark-type "Mark1" 'left))
  (define mtype2 (mark-type "Mark2" 'right))

  (define demo-rope
    (let ((r (lambda (s ms L R)
               (define m
                 (for/fold [(r (piece->rope s))] [(e ms)]
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
            (r "abc" m1 (rope-empty) (rope-empty))
            (r "ghi" m1 (rope-empty) (rope-empty)))
         (r "pqr"
            m1
            (r "mno" m1 (rope-empty) (rope-empty))
            (r "stu" m1 (rope-empty) (rope-empty))))))

  (check-equal? (rope->searchable-string demo-rope) "abcdefghijklmnopqrstu")
  (check-equal? (read-line (rope->searchable-port demo-rope)) "abcdefghijklmnopqrstu")

  (check-equal? (find-all-marks/type demo-rope mtype1)
                (for/hash ((i '(0 1 3 4 6 7 9 10 12 13 15 16 18 19))) (values i #t)))
  (check-equal? (find-all-marks/type demo-rope mtype2)
                (for/hash ((i '(3 6 9 12 15 18 21))) (values i #t)))

  (check-equal? (find-all-marks/type (clear-all-marks/type demo-rope mtype1) mtype1)
                (hash))
  (check-equal? (find-all-marks/type (clear-all-marks/type demo-rope mtype2) mtype1)
                (for/hash ((i '(0 1 3 4 6 7 9 10 12 13 15 16 18 19))) (values i #t)))

  (let ((is '(0 1 3 4 6 7 9 10 12 13 15 16 18 19)))
    (for ((i is))
      (define r (clear-mark demo-rope mtype1 i))
      (check-equal? (rope->searchable-string r) "abcdefghijklmnopqrstu")
      (check-equal? (find-all-marks/type r mtype1)
                    (for/hash ((j (remove i is))) (values j #t)))))

  (let ((is '(3 6 9 12 15 18 21)))
    (for ((i is))
      (define r (clear-mark demo-rope mtype2 i))
      (check-equal? (find-all-marks/type r mtype2)
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
    (define rope-pieces (map piece->rope string-pieces))
    (define text (string-append* string-pieces))
    (check-equal? (rope->searchable-string (car rope-pieces)) (car string-pieces))
    (check-equal? (rope->searchable-string (rope-concat rope-pieces)) text)
    (check-equal? (rope-size (rope-concat rope-pieces)) (string-length text))

    (check-eq? (rope-append (rope-empty) (car rope-pieces)) (car rope-pieces))
    (check-eq? (rope-append (car rope-pieces) (rope-empty)) (car rope-pieces))

    (let loop ((n 1000) (r0 (rope-concat rope-pieces)))
      (when (positive? n)
        (define pos (random (+ (rope-size r0) 1)))
        ;; (pretty-print (list pos r0))
        (define r (splay-to 'test-with-pieces r0 pos))
        (check-equal? (rope->searchable-string r) text)
        (loop (- n 1) r)))

    (let*-values (((r) (set-mark (rope-concat rope-pieces) mtype1 9 "original"))
                  ((_) (check-equal? (rope->searchable-string r) text))
                  ((pos val) (find-in-index/values r mtype1))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "original"))
                  ((r) (clear-mark r mtype1 pos))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash)))
                  ((pos val) (find-in-index/values r mtype1))
                  ((_) (check-false pos))
                  ((_) (check-false val))
                  ((r) (set-mark r mtype1 9 "second"))
                  ((pos val) (find-in-index/values r mtype1))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "second"))
                  ((r) (set-mark r mtype1 6 "first"))
                  ((r) (set-mark r mtype2 6 "third"))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash 6 "first" 9 "second")))
                  ((_) (check-equal? (find-all-marks/type r mtype2) (hash 6 "third")))
                  ((pos val) (find-in-index/values r mtype1 #:forward? #f))
                  ((_) (check-equal? pos 9))
                  ((_) (check-equal? val "second"))
                  ((pos val) (find-in-index/values r mtype1))
                  ((_) (check-equal? pos 6))
                  ((_) (check-equal? val "first"))
                  ((l r) (rope-split r pos))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash 3 "second")))
                  ((_) (check-equal? (find-all-marks/type l mtype1) (hash 6 "first")))
                  ((_) (check-equal? (find-all-marks/type r mtype2) (hash 0 "third")))
                  ((_) (check-equal? (find-all-marks/type l mtype2) (hash)))
                  ((_) (check-equal? (rope->searchable-string l) (substring text 0 6)))
                  ((_) (check-equal? (rope->searchable-string r) (substring text 6 (string-length text))))
                  ((_) (check-equal? (rope-index l) (marks-index (seteq mtype1))))
                  ((_) (check-equal? (rope-index r) (marks-index (seteq mtype1 mtype2))))
                  ((l r) (rope-split r 3))
                  ((_) (check-equal? (find-all-marks/type r mtype1) (hash)))
                  ((_) (check-equal? (find-all-marks/type l mtype1) (hash 3 "second")))
                  ((_) (check-equal? (find-all-marks/type r mtype2) (hash)))
                  ((_) (check-equal? (find-all-marks/type l mtype2) (hash 0 "third")))
                  ((_) (check-equal? (rope->searchable-string l) (substring text 6 9)))
                  ((_) (check-equal? (rope->searchable-string r) (substring text 9 (string-length text)))))
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

  (check-equal? (call-with-values (lambda () (rope-split (rope-empty) 0)) list)
                (list (rope-empty) (rope-empty)))

  (check-equal? (map rope->searchable-string
                     (call-with-values (lambda () (rope-split (piece->rope "abc") 0)) list))
                (list "" "abc"))
  (check-equal? (map rope->searchable-string
                     (call-with-values (lambda () (rope-split (piece->rope "abc") 2)) list))
                (list "ab" "c"))
  (check-equal? (map rope->searchable-string
                     (call-with-values (lambda () (rope-split (piece->rope "abc") 3)) list))
                (list "abc" ""))

  (check-equal? (map (lambda (i) (compute-range-index i 'default 10))
                     (list 0 10 3 -1 -2 11 12 -8 -9 -10 -11 -12))
                (list      0 10 3  9  8 10 10  2  1   0   0   0))

  (let* ((r (rope-append (piece->rope (make-string 10 #\a))
                         (piece->rope (make-string (* 2 *strand:glom-up-to*) #\z))))
         (expected-size (+ 10 (* 2 *strand:glom-up-to*)))
         (_ (check-equal? (rope-size r) expected-size))
         (r (set-mark r mtype1 (rope-size r) #t))
         (r (splay-to 'testing r 0))
         (pos (find-pos-in-index r mtype1)))
    (check-equal? pos expected-size))

  (let*-values (((r) (piece->rope "hello"))
                ((r) (set-mark r mtype2 (rope-size r) #t))
                ((l r) (rope-split r (find-pos-in-index r mtype2)))
                ((_) (check-equal? (rope->searchable-string l) "hello"))
                ((_) (check-equal? (rope->searchable-string r) ""))
                ((_) (check-equal? (rope-index l) #f))
                ((_) (check-equal? (rope-index r) (marks-index (seteq mtype2)))))
    (void))

  (let*-values (((xs) (make-string 128 #\x))
                ((r) (piece->rope (string-append "hello " xs)))
                ((r) (set-mark r mtype2 3 #t))
                ((l mr) (rope-split r (find-pos-in-index r mtype2)))
                ((m r) (rope-split mr 1))
                ((_) (check-equal? (rope->searchable-string l) "hel"))
                ((_) (check-equal? (rope->searchable-string m) "l"))
                ((_) (check-equal? (rope->searchable-string r) (string-append "o " xs)))
                ((_) (check-equal? (rope-index l) #f))
                ((_) (check-equal? (rope-index m) (marks-index (seteq mtype2))))
                ((_) (check-equal? (rope-index r) #f))
                ((new-m) (set-mark (rope-empty) mtype2 0 #t))
                ((r) (rope-append (rope-append l new-m) r))
                ((_) (check-equal? (rope->searchable-string r) (string-append "helo " xs)))
                ((_) (check-equal? (find-pos-in-index r mtype2) 3))
                ((r) (clear-mark r mtype2 (find-pos-in-index r mtype2)))
                ((_) (check-equal? (find-pos-in-index r mtype2) #f)))
    (void))

  (check-equal? (read (rope->searchable-port (piece->rope "(a b c)"))) '(a b c))
  (check-equal? (read-line (rope->searchable-port (piece->rope "(a b c)") #:forward? #f)) ")c b a(")
  (check-true (eof-object? (read (rope->searchable-port (rope-empty)))))
  )

