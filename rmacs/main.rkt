#lang racket/base

(require "display.rkt")
(require racket/match)

(define (main)
  (define t (stdin-tty))
  (tty-style t #:bold? #t #:background-color color-blue)
  (tty-display t (format "Your screen is ~a rows and ~a columns.\r\n"
                         (tty-rows t)
                         (tty-columns t)))
  (tty-style t #:bold? #f #:italic? #t)
  (tty-display t "Italic.\r\n")
  (tty-style t #:bold? #t)
  (tty-display t "Bold and italic.\r\n")
  (tty-style t #:bold? #f #:italic? #f)
  (tty-display t "Neither.\r\n"))

(module+ main
  (void (main)))
