#lang racket/base
;; Knuth-Morris-Pratt string & rope search.

(provide search-generator
         search-string
         search-rope
         findf-in-generator
         findf-in-rope)

(require racket/set)
(require racket/match)
(require "rope.rkt")

(define (table pattern)
  (define limit (- (string-length pattern) 1))
  (if (positive? limit)
      (let ((t (make-vector limit)))
        (vector-set! t 0 0)
        (let loop ((pos 1) (candidate 0))
          (cond
           [(= pos limit)
            t]
           [(equal? (string-ref pattern pos)
                    (string-ref pattern candidate))
            (vector-set! t pos (+ candidate 1))
            (loop (+ pos 1)
                  (+ candidate 1))]
           [(> candidate 0)
            (loop pos
                  (vector-ref t candidate))]
           [else
            (vector-set! t pos 0)
            (loop (+ pos 1)
                  0)])))
      #f))

;; String (Generator Char) -> (Option Index)
(define (search-generator needle haystack)
  (define t (table needle))
  (define cache #f)
  (define (advance!)
    (define next (haystack))
    (set! cache (and (char? next) next)))
  (advance!)
  (let loop ((m 0) (i 0))
    (cond
     [(not cache)
      #f]
     [(equal? (string-ref needle i) cache)
      (if (= i (- (string-length needle) 1))
          m
          (begin (advance!)
                 (loop m (+ i 1))))]
     [(> i 0)
      (define ti (vector-ref t (- i 1)))
      (loop (- (+ m i) ti) ti)]
     [else
      (advance!)
      (loop (+ m 1) i)])))

;; String String -> (Option Index)
(define (search-string needle haystack)
  (define t (table needle))
  (let loop ((m 0) (i 0))
    (cond
     [(= (+ m i) (string-length haystack))
      #f]
     [(equal? (string-ref needle i) (string-ref haystack (+ m i)))
      (if (= i (- (string-length needle) 1))
          m
          (loop m (+ i 1)))]
     [(> i 0)
      (define ti (vector-ref t (- i 1)))
      (loop (- (+ m i) ti) ti)]
     [else
      (loop (+ m 1) i)])))

;; String Rope -> (Option Index)
(define (search-rope needle haystack #:forward? [forward? #t])
  (if forward?
      (search-generator needle (rope-generator haystack))
      (let ((reversed-result (search-generator (list->string (reverse (string->list needle)))
                                               (rope-generator haystack #:forward? #f))))
        (and reversed-result (- (rope-size haystack) reversed-result (string-length needle))))))

(define (findf-in-generator f gen)
  (let loop ((count 0))
    (match (gen)
      [(? char? c)
       (if (f c)
           count
           (loop (+ count 1)))]
      [_ count])))

(define (findf-in-rope f r #:forward? [forward? #t])
  (if forward?
      (findf-in-generator f (rope-generator r))
      (- (rope-size r) (findf-in-generator f (rope-generator r #:forward? #f)))))

(module+ test
  (require rackunit)
  (require racket/generator)
  (check-equal? (table "ABCDABD")
                (vector 0 0 0 0 1 2))
  (check-equal? (table "PARTICIPATE IN PARACHUTE")
                (vector 0 0 0 0 0 0 0 1 2 0 0 0 0 0 0 1 2 3 0 0 0 0 0))
  (check-equal? (search-string "ABCDABD" "ABC ABCDAB ABCDABCDABDE") 15)
  (check-equal? (search-string "AAAA" "AAABAAABAAABAAABAAAB") #f)

  (check-equal? (search-generator "ABCDABD" (sequence->generator "ABC ABCDAB ABCDABCDABDE")) 15)
  (check-equal? (search-generator "AAAA" (sequence->generator "AAABAAABAAABAAABAAAB")) #f)

  (define prejudice-rope
    (rope-concat
     (map string->rope
          (list "It is a truth universally acknowledged, that a single man in possession of a good fortune must be in want of a wife.\n"
                "\n"
                "However little known the feelings or views of such a man may be on his first entering a neighbourhood, this truth is so well fixed in the minds of the surrounding families, that he is considered as the rightful property of some one or other of their daughters.\n"
                "\n"
                "``My dear Mr. Bennet,'' said his lady to him one day, ``have you heard that Netherfield Park is let at last?''\n"
                "\n"
                "Mr. Bennet replied that he had not.\n"))))

  (check-equal? (search-rope "man" prejudice-rope) 54)
  (check-equal? (search-rope "man" prejudice-rope #:forward? #f) 171)
  (check-equal? (search-rope "man in" prejudice-rope) 54)
  (check-equal? (search-rope "man may" prejudice-rope) 171)
  (check-equal? (search-rope "man may" prejudice-rope #:forward? #f) 171)
  (check-equal? (search-rope "xylophone" prejudice-rope) #f)
  (check-equal? (search-rope "xylophone" prejudice-rope #:forward? #f) #f)

  (define (find-in-rope delims r #:forward? [forward? #t])
    (define chs (list->set (string->list delims)))
    (findf-in-rope (lambda (c) (set-member? chs c)) r #:forward? forward?))

  (check-equal? (find-in-rope "\n" prejudice-rope) 116)
  (check-equal? (find-in-rope "at" prejudice-rope) 1)
  (check-equal? (find-in-rope "z" prejudice-rope) (rope-size prejudice-rope))
  (check-equal? (find-in-rope "\n" prejudice-rope #:forward? #f) (rope-size prejudice-rope))
  (check-equal? (find-in-rope "at" prejudice-rope #:forward? #f) (- (rope-size prejudice-rope) 2))
  (check-equal? (find-in-rope "z" prejudice-rope #:forward? #f) 0))
