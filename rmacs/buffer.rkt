#lang racket/base

(provide main-mark-type
         buffer?
         make-buffer
         buffer-size
         buffer-move-to!
         buffer-move-by!
         buffer-mark!
         buffer-clear-mark!
         buffer-mark-pos
         buffer-region-split
         buffer-region
         buffer-region-update!
         call-with-excursion
         buffer-search
         buffer-find)

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
  (buffer-lift rope-seek buf pos))

(define (buffer-move-by! buf delta)
  (buffer-move-to! buf (+ (buffer-pos buf) delta)))

(define (buffer-mark! buf [mtype main-mark-type] #:position [pos (buffer-pos buf)] #:value [value #t])
  (buffer-lift replace-mark buf mtype pos value))

(define (buffer-clear-mark! buf [mtype main-mark-type])
  (define pos (find-mark-pos (buffer-rope buf) mtype))
  (if pos
      (buffer-lift clear-mark buf mtype pos)
      buf))

(define (buffer-mark-pos buf [mtype main-mark-type])
  (find-mark-pos (buffer-rope buf) mtype))

(define (buffer-region-split* buf pos mark)
  (define lo (min pos mark))
  (define hi (max pos mark))
  (define-values (l mr) (rope-split (buffer-rope buf) lo))
  (define-values (m r) (rope-split mr (- hi lo)))
  (values l m r))

(define (buffer-region-split buf
                             #:point [pos (buffer-pos buf)]
                             #:mark [mark (buffer-mark-pos buf)])
  (buffer-region-split* buf pos mark))

(define (buffer-region buf
                       #:point [pos (buffer-pos buf)]
                       #:mark [mark (buffer-mark-pos buf)])
  (define-values (_l m _r) (buffer-region-split* buf pos mark))
  m)

(define (buffer-region-update! buf updater
                               #:point [pos (buffer-pos buf)]
                               #:mark [mark (buffer-mark-pos buf)])
  (define-values (l m r) (buffer-region-split* buf pos mark))
  (set-buffer-rope! buf (rope-concat (list l (updater m) r)))
  buf)

(define (call-with-excursion buf f)
  (define excursion (gensym 'excursion))
  (define saved-mark-type (mark-type (format "Saved mark ~a" excursion) 'right))
  (define saved-point-type (mark-type (format "Saved point ~a" excursion) 'right))
  (buffer-mark! buf saved-mark-type #:position (buffer-mark-pos buf))
  (buffer-mark! buf saved-point-type #:position (buffer-pos buf))
  (define (restore!)
    (define restore-mark-pos (buffer-mark-pos buf saved-mark-type))
    (define restore-point-pos (buffer-mark-pos buf saved-point-type))
    (when restore-mark-pos (buffer-mark! buf #:position restore-mark-pos))
    (when restore-point-pos (buffer-move-to! buf restore-point-pos))
    (buffer-clear-mark! buf saved-mark-type)
    (buffer-clear-mark! buf saved-point-type))
  (with-handlers [(exn? (lambda (e)
                          (restore!)
                          (raise e)))]
    (define result (f))
    (restore!)
    result))

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

(define (buffer-lift f buf . args)
  (define new-rope (apply f (buffer-rope buf) args))
  (set-buffer-rope! buf new-rope)
  buf)