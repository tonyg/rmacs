#lang racket/base
;; ANSI/VT10x escape sequences
;; Based initially on http://en.wikipedia.org/wiki/ANSI_escape_code.
;;
;; This is also an excellent resource:
;; http://bjh21.me.uk/all-escapes/all-escapes.txt

(provide (except-out (all-defined-out)
		     CSI
		     define-escape-sequence))

(define CSI "\033[")

(define-syntax-rule (define-escape-sequence (name arg ...) piece ...)
  (define (name arg ...)
    (let ((arg (number->string arg)) ...)
      (string-append piece ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic ANSI sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-escape-sequence (insert-characters n) CSI n "@")

(define-escape-sequence (move-cursor-up n) CSI n "A")
(define-escape-sequence (move-cursor-down n) CSI n "B")
(define-escape-sequence (move-cursor-right n) CSI n "C")
(define-escape-sequence (move-cursor-left n) CSI n "D")

(define-escape-sequence (cursor-next-line n) CSI n "E")
(define-escape-sequence (cursor-previous-line n) CSI n "F")

(define-escape-sequence (goto-column n) CSI n "G")
(define-escape-sequence (goto row column) CSI row ";" column "H")

(define-escape-sequence (cursor-forward-tabulation n) CSI n "I")

(define-escape-sequence (clear-screen-from-cursor) CSI "0J")
(define-escape-sequence (clear-screen-to-cursor) CSI "1J")
(define-escape-sequence (clear-screen) CSI "2J")

(define-escape-sequence (clear-to-eol) CSI "0K")
(define-escape-sequence (clear-to-sol) CSI "1K")
(define-escape-sequence (clear-line) CSI "2K")

(define-escape-sequence (insert-lines n) CSI n "L")
(define-escape-sequence (delete-lines n) CSI n "M")

(define-escape-sequence (clear-to-end-of-field) CSI "0N")
(define-escape-sequence (clear-to-start-of-field) CSI "1N")
(define-escape-sequence (clear-field) CSI "2N")

(define-escape-sequence (clear-to-end-of-area) CSI "0O")
(define-escape-sequence (clear-to-start-of-area) CSI "1O")
(define-escape-sequence (clear-area) CSI "2O")

(define-escape-sequence (delete-characters n) CSI n "P")

(define-escape-sequence (select-editing-extent-page) CSI "0Q")
(define-escape-sequence (select-editing-extent-line) CSI "1Q")
(define-escape-sequence (select-editing-extent-field) CSI "2Q")
(define-escape-sequence (select-editing-extent-area) CSI "3Q")
(define-escape-sequence (select-editing-extent-whole) CSI "4Q")

(define-escape-sequence (active-position-report row column) CSI row ";" column "R")

(define-escape-sequence (scroll-up n) CSI n "S")
(define-escape-sequence (scroll-down n) CSI n "T")

(define-escape-sequence (next-page n) CSI n "U")
(define-escape-sequence (previous-page n) CSI n "V")

(define (select-graphic-rendition . parameters)
  (if (null? parameters)
      (string-append CSI "m")
      (string-append CSI
		     (number->string (car parameters))
		     (foldr (lambda (n acc) (string-append ";" (number->string n) acc))
			    "m"
			    (cdr parameters)))))

(define-escape-sequence (device-status-report) CSI "6n")
(define-escape-sequence (save-cursor-position) CSI "s")
(define-escape-sequence (restore-cursor-position) CSI "u")
(define-escape-sequence (hide-cursor) CSI "?25l")
(define-escape-sequence (show-cursor) CSI "?25h")

(define-escape-sequence (ansi-interrupt) "\033a")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEC private VT100 sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-escape-sequence (dec-double-width-double-height-top) "\033#3")
(define-escape-sequence (dec-double-width-double-height-bottom) "\033#4")
(define-escape-sequence (dec-single-width-single-height) "\033#5")
(define-escape-sequence (dec-double-width-single-height) "\033#6")

(define-escape-sequence (dec-save-cursor) "\0337")
(define-escape-sequence (dec-restore-cursor) "\0338")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters for select-graphic-rendition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define style-normal 0)
(define style-bold 1)
(define style-faint 2)
(define style-italic/inverse 3)
(define style-underline 4)
(define style-blink-slow 5)
(define style-blink-fast 6)
(define style-inverse 7)
(define style-conceal 8)
(define style-crossed-out 9)

(define style-primary-font 10)
(define (style-font n)
  (if (<= 0 n 9)
      (+ n 10)
      (error 'style-font "Font number out of range: ~a" n)))

(define style-fraktur 20)
(define style-no-bold/double-underline 21)
(define style-normal-intensity 22)
(define style-no-italic-no-fraktur 23)
(define style-no-underline 24)
(define style-no-blink 25)
;; 26 reserved, per wikipedia
(define style-no-inverse 27)
(define style-no-conceal 28)

(define (style-text-color n)
  (if (<= 0 n 7)
      (+ n 30)
      (error 'style-text-color "Color number out of range: ~a" n)))
(define style-default-text-color 39)

(define (style-background-color n)
  (if (<= 0 n 7)
      (+ n 40)
      (error 'style-background-color "Color number out of range: ~a" n)))
(define style-default-background-color 49)

(define-escape-sequence (select-xterm-256-text-color n) CSI "38;5;" n "m")
(define-escape-sequence (select-xterm-256-background-color n) CSI "48;5;" n "m")

;; 50 reserved, per wikipedia
(define style-framed 51)
(define style-encircled 52)
(define style-overlined 53)
(define style-no-framed-no-encircled 54)
(define style-no-overlined 55)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors for certain parameters to select-graphic-rendition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define color-black 0)
(define color-red 1)
(define color-green 2)
(define color-yellow 3)
(define color-blue 4)
(define color-magenta 5)
(define color-cyan 6)
(define color-white 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derived sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-escape-sequence (kill-line)
  (goto-column 1)
  (clear-to-eol))

(define-escape-sequence (clear-screen/home)
  (clear-screen)
  (goto 1 1))
