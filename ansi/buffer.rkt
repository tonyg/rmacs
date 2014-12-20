#lang racket/base

(require "rope.rkt")
(require "search.rkt")

(define main-mark-type (mark-type "main" 'right))

(struct buffer ([rope #:mutable]
                [pos #:mutable]
                ) #:transparent)

(define (make-buffer #:initial-contents [initial-contents ""])
  (buffer (string->rope initial-contents)
          0))

(define (buffer-size buf) (rope-size (buffer-rope buf)))

(define (buffer-move-to! buf pos)
  (set-buffer-pos! buf (max 0 (min (buffer-size buf) pos)))
  (buffer-seek! buf pos))

(define (buffer-seek! buf pos)
  (buffer-lift0 rope-seek buf pos))

(define (buffer-move-by! buf delta)
  (buffer-move-to! buf (+ (buffer-pos buf) delta)))

(define (buffer-mark! buf [mtype main-mark-type] #:position [pos (buffer-pos buf)] #:value [value #t])
  (buffer-lift0 replace-mark buf mtype pos value))

(define (buffer-search* buf start-pos forward? move? find-delta)
  (define-values (l r) (rope-split (buffer-rope buf) start-pos))
  (define delta (find-delta (if forward? r l)))
  (define new-pos (+ start-pos (cond [(not delta) 0] [forward? delta] [else (- delta)])))
  (when delta
    (if move?
        (buffer-move-to! buf new-pos)
        (buffer-seek! buf new-pos)))
  new-pos)

(define (buffer-search buf needle
                       #:position [start-pos (buffer-pos buf)]
                       #:forward? [forward? #t]
                       #:move? [move? #t])
  (buffer-search* buf start-pos forward? move?
                  (lambda (piece) (search-rope needle piece #:forward? forward?))))

(define (buffer-find buf delims
                     #:position [start-pos (buffer-pos buf)]
                     #:forward? [forward? #t]
                     #:move? [move? #t])
  (buffer-search* buf start-pos forward? move?
                  (lambda (piece) (find-in-rope delims piece #:forward? forward?))))

(define (buffer-lift0 f buf . args)
  (define new-rope (apply f (buffer-rope buf) args))
  (set-buffer-rope! buf new-rope)
  buf)

(define (buffer-lift1 f buf . args)
  (define-values (result new-rope) (apply f (buffer-rope buf) args))
  (set-buffer-rope! buf new-rope)
  result)

;; Finseth's book defines a C routine, Framer(), which is intended to
;; ensure that the `top_of_window` mark, denoting the position where
;; display should begin for the current window, is in a sane position.
;; The mark is left alone unless the cursor is outside the currently
;; displayed window, either above or below. If the mark needs to be
;; moved, it is moved to a line such that the cursor, after redisplay,
;; will end up at a configurable percentage of the way down the
;; window.
;;
;; MarkType Location Buffer -> Buffer
;; Ensures the given mark is sanely positioned as a top-of-window mark
;; with respect to the given cursor position.
(define (frame-buffer! top-of-window-mtype cursor-position window-height buf
                       #:preferred-position-fraction [preferred-position-fraction 1/2])
  (define old-top-of-window-pos (find-next-mark-pos (buffer-rope buf) top-of-window-mtype))
  (define preferred-distance-from-bottom (ceiling (* window-height (- 1 preferred-position-fraction))))
  (let loop ((pos (buffer-find buf "\n" #:forward? #f #:move? #f))
             (line-count 0)
             (top-of-window-pos old-top-of-window-pos))
    (define new-top-of-window-pos
      (if (= line-count preferred-distance-from-bottom) pos top-of-window-pos))
    (cond
     [(<= pos old-top-of-window-pos)
      buf]
     [(= line-count window-height)
      (buffer-mark! buf top-of-window-mtype #:position new-top-of-window-pos)]
     [else
      (loop (buffer-find buf "\n" #:forward? #f #:move? #f #:position (- pos 1))
            (+ line-count 1)
            new-top-of-window-pos)])))
