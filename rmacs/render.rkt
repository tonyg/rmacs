#lang racket/base

(provide (struct-out absolute-size)
         (struct-out relative-size)
         render-windows!)

(require racket/match)

(require "buffer.rkt")
(require "window.rkt")
(require "display.rkt")
(require "rope.rkt")

;; A SizeSpec is either
;; -- (absolute-size PositiveInteger), a specific size in screen rows
;; -- (relative-size PositiveReal), a weighted window size
(struct absolute-size (lines) #:prefab)
(struct relative-size (weight) #:prefab)

(define (newline? c) (equal? c #\newline))
(define (not-newline? c) (not (newline? c)))

;; Finseth's book defines a C routine, Framer(), which is intended to
;; ensure that the `top_of_window` mark, denoting the position where
;; display should begin for the current window, is in a sane position.
;; The mark is left alone unless the cursor is outside the currently
;; displayed window, either above or below. If the mark needs to be
;; moved, it is moved to a line such that the cursor, after redisplay,
;; will end up at a configurable percentage of the way down the
;; window.
;;
;; Window Nat -> Nat
;; Ensures that window-top is sanely positioned with respect to
;; window-point. Returns the new position of window-top.
(define (frame! win available-line-count
                #:preferred-position-fraction [preferred-position-fraction 1/2])
  (define buf (window-buffer win))
  (define old-top-of-window-pos (or (buffer-mark-pos* buf (window-top win)) 0))
  (define preferred-distance-from-bottom
    (ceiling (* available-line-count (- 1 preferred-position-fraction))))
  (let loop ((pos (buffer-findf buf (window-point win) newline? #:forward? #f))
             (line-count 0)
             (top-of-window-pos old-top-of-window-pos))
    (define new-top-of-window-pos
      (if (= line-count preferred-distance-from-bottom) pos top-of-window-pos))
    (cond
     [(= pos old-top-of-window-pos)
      old-top-of-window-pos]
     [(>= line-count (- available-line-count 1))
      (buffer-mark! buf (window-top win) new-top-of-window-pos)
      new-top-of-window-pos]
     [else
      (loop (buffer-findf buf (- pos 1) newline? #:forward? #f)
            (+ line-count 1)
            new-top-of-window-pos)])))

(define (tty-body-style t is-active?)
  (tty-set-pen! t tty-default-pen))

(define (tty-statusline-style t is-active?)
  (tty-set-pen! t (pen color-black color-white #f #f)))

(define (format-line line window-width cursor-input-pos)
  (let loop ((chars (string->list line))
             (remaining-width window-width)
             (cursor-input-pos cursor-input-pos)
             (acc-rev '())
             (cursor-offset (if (zero? cursor-input-pos) 0 #f)))
    (define (finish) (values (list->string (reverse acc-rev))
                             (if (zero? cursor-input-pos) (length acc-rev) cursor-offset)))
    (match chars
      ['() (finish)]
      [(cons c rest)
       (define (emit str)
         (define needed (string-length str))
         (if (>= remaining-width needed)
             (loop rest
                   (- remaining-width needed)
                   (- cursor-input-pos 1)
                   (append (reverse (string->list str)) acc-rev)
                   (if (zero? cursor-input-pos)
                       (length acc-rev)
                       cursor-offset))
             (finish)))
       (match c
         [#\tab
          (emit (make-string (- 8 (modulo (length acc-rev) 8)) #\space))]
         [(? char-iso-control?)
          (emit (format "[~x]" (char->integer c)))]
         [_
          (emit (string c))])])))

(define (render-window! t win window-top window-height is-active?)
  (define buf (window-buffer win))
  (define available-line-count (- window-height 1))
  (define top-of-window-pos (frame! win available-line-count))
  (define cursor-pos (buffer-mark-pos buf (window-point win)))
  (tty-goto t window-top 0)
  (tty-body-style t is-active?)
  (define cursor-coordinates
    (let loop ((line-count 0)
               (sol-pos top-of-window-pos)
               (cursor-coordinates #f))
      (cond
       [(>= line-count available-line-count)
        cursor-coordinates]
       [else
        (define eol-pos (buffer-findf buf sol-pos newline?))
        (define line (rope->string (buffer-region buf sol-pos eol-pos)))
        (define-values (formatted-line cursor-offset)
          (format-line line (tty-columns t) (- cursor-pos sol-pos)))
        (tty-display t formatted-line)
        (tty-clear-to-eol t)
        (tty-newline t)
        (loop (+ line-count 1)
              (+ eol-pos 1)
              (if cursor-offset
                  (list (+ line-count window-top) cursor-offset)
                  cursor-coordinates))])))
  (tty-statusline-style t is-active?)
  (tty-display t "-- " (buffer-title buf) " ")
  (let ((remaining-length (- (tty-columns t) 4 (string-length (buffer-title buf)))))
    (when (positive? remaining-length)
      (tty-display t (make-string remaining-length #\-))))
  cursor-coordinates)

(define (layout-windows ws total-height [minimum-height 4])
  (define total-weight (foldl + 0 (map (lambda (e)
                                         (match (cadr e)
                                           [(absolute-size _) 0]
                                           [(relative-size w) w])) ws)))
  (define reserved-lines (foldl + 0 (map (lambda (e)
                                           (match (cadr e)
                                             [(absolute-size lines) lines]
                                             [(relative-size _) 0])) ws)))
  (define proportional-lines (- total-height reserved-lines))
  (let loop ((ws ws) (offset 0) (remaining proportional-lines))
    (match ws
      ['() '()]
      [(cons (list w (absolute-size lines)) rest)
       (cons (list w offset lines) (loop rest (+ offset lines) remaining))]
      [(cons (list w (relative-size weight)) rest)
       (define height (max minimum-height
                           (inexact->exact
                            (round (* proportional-lines (/ weight total-weight))))))
       (if (>= remaining height)
           (if (null? rest)
               (list (list w offset remaining))
               (cons (list w offset height) (loop rest (+ offset height) (- remaining height))))
           (if (>= remaining minimum-height)
               (list (list w offset remaining))
               '()))])))

(define (render-windows! t ws active-window)
  (define layout (layout-windows ws (tty-rows t)))
  (tty-body-style t #f)
  (tty-goto t 0 0)
  (define active-cursor-position
    (for/fold [(cursor-position #f)] [(e layout)]
      (match-define (list w window-top window-height) e)
      (define is-active? (eq? w active-window))
      (define window-cursor-position (render-window! t w window-top window-height is-active?))
      (if is-active? window-cursor-position cursor-position)))
  (when active-cursor-position
    (tty-goto t (car active-cursor-position) (cadr active-cursor-position)))
  (tty-flush t))
