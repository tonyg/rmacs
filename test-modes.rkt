#lang racket/base

(require "ansi.rkt")

(define (display* . vs)
  (for-each display vs)
  (flush-output))

(display* (dec-soft-terminal-reset)
	  (select-graphic-rendition style-bold
				    (style-text-color color-yellow)
				    (style-background-color color-blue))
	  (clear-screen/home))

(display* (reset-mode send/receive-mode))

(display (read-char))
(display (read-char))
(display (read-char))

(display* (dec-soft-terminal-reset))
