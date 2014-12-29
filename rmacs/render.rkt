#lang racket/base

(provide (struct-out absolute-size)
         (struct-out relative-size)
         (struct-out layout)
         layout-windows
         render-windows!)

(require racket/match)

(require "buffer.rkt")
(require "window.rkt")
(require "display.rkt")
(require "rope.rkt")
(require "wrap.rkt")

;; A SizeSpec is either
;; -- (absolute-size PositiveInteger), a specific size in screen rows
;; -- (relative-size PositiveReal), a weighted window size
(struct absolute-size (lines) #:prefab)
(struct relative-size (weight) #:prefab)

;; A Layout is a (layout Window SizeSpec Nat Nat)
(struct layout (window ;; Window
                size-spec ;; SizeSpec
                top ;; Nat, a row
                left ;; Nat, a column
                width ;; Nat
                height ;; Nat
                ) #:prefab)

(define (newline? c) (equal? c #\newline))

;; Finseth's book defines a C routine, Framer(), which is intended to
;; ensure that the `top_of_window` mark, denoting the position where
;; display should begin for the current window, is in a sane position.
;; The mark is left alone unless the cursor is outside the currently
;; displayed window, either above or below. If the mark needs to be
;; moved, it is moved to a line such that the cursor, after redisplay,
;; will end up at a configurable percentage of the way down the
;; window.
;;
;; It is here that we perform soft-wrapping of lines.
;;
;; Window Nat -> (List (Pair Nat Nat))
;; Ensures that window-top is sanely positioned with respect to
;; window-point. Returns wrapped line spans starting at the new
;; window-top.
(define (frame! win available-line-count window-width
                #:preferred-position-fraction [preferred-position-fraction 1/2])
  (define buf (window-buffer win))
  (define old-top-of-window-pos (or (buffer-mark-pos* buf (window-top win)) 0))
  (define preferred-distance-from-bottom
    (ceiling (* available-line-count (- 1 preferred-position-fraction))))
  (define g (buffer-lines-reverse/wrap buf (window-point win) basic-wrap window-width))
  (define spans
    (let loop ((line-count 0)
               (all-spans '())
               (preferred-spans '()))
      (define-values (pos eol-pos) (g))
      (define span (cons pos eol-pos))
      (define new-all-spans (cons span all-spans))
      (define new-preferred-spans (if (= line-count preferred-distance-from-bottom)
                                      new-all-spans
                                      preferred-spans))
      (cond
       [(not pos) all-spans] ;; we hit buffer top before our preferred distance. NB all-spans
       [(= pos old-top-of-window-pos) new-all-spans]
       [(>= line-count (- available-line-count 1)) new-preferred-spans]
       [else (loop (+ line-count 1) new-all-spans new-preferred-spans)])))
  (buffer-mark! buf (window-top win) (caar spans))
  spans)

(define (tty-body-style t is-active?)
  (tty-set-pen! t tty-default-pen))

(define (tty-statusline-style t is-active?)
  (tty-set-pen! t (pen color-black color-white #f #f)))

(define (render-window! t win window-top window-width window-height is-active?)
  (define buf (window-buffer win))
  (define available-line-count (if (window-status-line? win) (- window-height 1) window-height))
  (define spans (frame! win available-line-count window-width))
  (define cursor-pos (buffer-mark-pos buf (window-point win)))
  (tty-goto t window-top 0)
  (tty-body-style t is-active?)

  (define (render-span sol-pos eol-pos line-count cursor-coordinates)
    (define line (rope->string (buffer-region buf sol-pos eol-pos)))
    (tty-display t line)
    (tty-newline t)
    (if (<= sol-pos cursor-pos eol-pos)
        (list (+ line-count window-top)
              (let ((line-to-cursor (substring line 0 (- cursor-pos sol-pos))))
                (buffer-string-column-count buf 0 line-to-cursor)))
        cursor-coordinates))

  (define (render-top-spans spans line-count cursor-coordinates)
    (cond
     [(>= line-count available-line-count) cursor-coordinates]
     [(null? spans)
      (define g (buffer-lines-forward/wrap buf (window-point win) basic-wrap window-width))
      (g) ;; discard first span, since it has already been covered
      (render-bottom-spans g line-count cursor-coordinates)]
     [else
      (render-top-spans (cdr spans)
                        (+ line-count 1)
                        (render-span (caar spans) (cdar spans) line-count cursor-coordinates))]))

  (define (render-bottom-spans g line-count cursor-coordinates)
    (if (>= line-count available-line-count)
        cursor-coordinates
        (let-values (((sol-pos eol-pos) (g)))
          (if sol-pos
              (render-bottom-spans g
                                   (+ line-count 1)
                                   (render-span sol-pos eol-pos line-count cursor-coordinates))
              (begin (for ((i (- available-line-count line-count))) (tty-newline t))
                     cursor-coordinates)))))

  (define cursor-coordinates (render-top-spans spans 0 #f))

  (when (window-status-line? win)
    (tty-statusline-style t is-active?)
    (let* ((prefix (format "-:~a- ~a " (if (buffer-dirty? buf) "**" "--") (buffer-title buf)))
           (remaining-length (- (tty-columns t) (string-length prefix))))
      (tty-display t prefix)
      (when (positive? remaining-length) (tty-display t (make-string remaining-length #\-)))))

  cursor-coordinates)

(define (layout-windows ws miniwin total-width total-height [minimum-height 4])
  (define miniwin-spans
    (frame! miniwin (min 4 total-height) total-width #:preferred-position-fraction 1))
  (define miniwin-height (length miniwin-spans))
  (define total-weight (foldl + 0 (map (lambda (e)
                                         (match (cadr e)
                                           [(absolute-size _) 0]
                                           [(relative-size w) w])) ws)))
  (define reserved-lines (foldl + miniwin-height (map (lambda (e)
                                                        (match (cadr e)
                                                          [(absolute-size lines) lines]
                                                          [(relative-size _) 0])) ws)))
  (define proportional-lines (- total-height reserved-lines))
  (define ws-without-miniwin ;; miniwin is in ws when minibuffer active; otherwise, not
    (filter (lambda (e) (not (eq? (car e) miniwin))) ws))
  (append (let loop ((ws ws-without-miniwin) (offset 0) (remaining proportional-lines))
            (match ws
              ['() '()]
              [(cons (list (== miniwin eq?) _) rest)
               (loop rest offset remaining)]
              [(cons (list w (and spec (absolute-size lines))) rest)
               (cons (layout w spec offset 0 total-width lines)
                     (loop rest (+ offset lines) remaining))]
              [(cons (list w (and spec (relative-size weight))) rest)
               (define height (max minimum-height
                                   (inexact->exact
                                    (round (* proportional-lines (/ weight total-weight))))))
               (if (>= remaining height)
                   (if (null? rest)
                       (list (layout w spec offset 0 total-width remaining))
                       (cons (layout w spec offset 0 total-width height)
                             (loop rest (+ offset height) (- remaining height))))
                   (if (>= remaining minimum-height)
                       (list (layout w spec offset 0 total-width remaining))
                       '()))]))
          (list (layout miniwin
                        (absolute-size miniwin-height)
                        (- total-height miniwin-height)
                        0
                        total-width
                        miniwin-height))))

(define (render-windows! t layouts active-window)
  (tty-body-style t #f)
  (tty-goto t 0 0)
  (define active-cursor-position
    (for/fold [(cursor-position #f)] [(e layouts)]
      (match-define (layout w _spec window-top _left window-width window-height) e)
      (define is-active? (eq? w active-window))
      (define window-cursor-position
        (render-window! t w window-top window-width window-height is-active?))
      (if is-active? window-cursor-position cursor-position)))
  (when active-cursor-position
    (tty-goto t (car active-cursor-position) (cadr active-cursor-position)))
  (tty-flush t))
