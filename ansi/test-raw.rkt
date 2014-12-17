#lang racket/base

(require "private/tty-raw-extension")
(require "main.rkt")

(require racket/set)
(require racket/match)

(define (main)
  (tty-raw!)

  (plumber-add-flush! (current-plumber)
                      (lambda (handle)
                        (display (reset-mode x11-any-event-mouse-tracking-mode))))

  ;; lcd-terminal isn't bright enough to parse mouse events yet, so this is disabled for now
  ;; (for-each display (list (set-mode x11-any-event-mouse-tracking-mode)))

  (display "Type keys. Press control-D to exit.\r\n")
  (let loop ()
    (flush-output)
    (match (lex-lcd-input (current-input-port))
      [(? eof-object?) (void)]
      [(== (key #\D (set 'control))) (void)]
      [key
       (printf "Key: ~v\r\n" key)
       (loop)])))

(main)
