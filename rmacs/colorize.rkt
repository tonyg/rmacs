#lang racket/base

(provide colorize-burst)

(require racket/match)
(require syntax-color/module-lexer)
(require syntax-color/lexer-contract)
(require "rope.rkt")
(require "buffer.rkt")
(require "render.rkt")
(require "display.rkt")

(define colorization-state-mark (mark-type (buffer-mark-type 'colorization-state #f #f) 'right))

(struct colorization-state (mode) #:prefab)

(define colors
  (hash 'comment (pen color-red color-black #f #t)
        'sexp-comment (pen color-red color-black #f #t)
        'white-space tty-default-pen
        'symbol tty-default-pen
        'string (pen color-yellow color-black #f #f)
        'constant (pen color-green color-black #f #f)
        'parenthesis tty-default-pen
        'error (pen color-white color-red #f #f)
        'hash-colon-keyword (pen color-cyan color-black #f #f)
        'other (pen color-magenta color-black #f #f)
        ))

(define (strip-colorization! buf)
  (buffer-clear-all-marks/type! buf color-mark 0 (buffer-size buf)))

(define (colorize-burst buf [start-pos0 (buffer-size buf)])
  (match-define-values (start-pos (colorization-state mode))
    (match (buffer-mark* buf colorization-state-mark #:forward? #f #:position start-pos0)
      [#f (values 0 (colorization-state #f))]
      [(cons pos s) (values pos s)]))
  (define-values (_l r) (rope-split (buffer-rope buf) start-pos))
  (define in (open-input-rope r))
  (define start-time (current-inexact-milliseconds))
  (let loop ((mode mode))
    (define-values (lexeme type data new-token-start new-token-end backup-delta new-mode)
      (module-lexer in start-pos mode))
    ;; (log-info "color ~v mode ~v from ~v gave ~v ~v between ~v-~v"
    ;;           (buffer-title buf)
    ;;           mode
    ;;           start-pos
    ;;           type
    ;;           lexeme
    ;;           new-token-start
    ;;           new-token-end)
    (when (not (zero? backup-delta))
      (log-warning "Non-zero backup-delta ~v seen while colorizing buffer ~v at position ~v"
                   backup-delta
                   (buffer-title buf)
                   start-pos))
    (if (eq? type 'eof)
        #f
        (let ((pen (hash-ref colors type)))
          (when (not (equal? pen tty-default-pen))
            (buffer-mark! buf color-mark (+ start-pos new-token-start -1)
                          #:value pen #:replace? #f)
            (buffer-mark! buf color-mark (+ start-pos new-token-end -1)
                          #:value tty-default-pen #:replace? #f))
          (if (dont-stop? new-mode)
              (loop (dont-stop-val new-mode))
              (begin
                (buffer-mark! buf colorization-state-mark (+ start-pos new-token-end -1)
                              #:value (colorization-state new-mode) #:replace? #f)
                (let ((now (current-inexact-milliseconds)))
                  (if (< (- now start-time) 50)
                      (loop new-mode)
                      #t))))))))
