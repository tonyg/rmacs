#lang racket/base
;; ANSI/VT10x escape sequences.
;;
;; Based initially on http://en.wikipedia.org/wiki/ANSI_escape_code,
;; but it doesn't have very many definitions; this page, however, is
;; comprehensive and excellent:
;; http://bjh21.me.uk/all-escapes/all-escapes.txt
;;
;; See also http://www.xfree86.org/current/ctlseqs.html

(provide (except-out (all-defined-out)
		     CSI
		     ST
		     OSC
		     format-parameter
		     define-variable-arity-escape-sequence
		     define-escape-sequence))

(define CSI "\033[")
(define ST  "\033\\")
(define OSC "\033]")

(define (format-parameter v)
  (cond
   ((number? v) (number->string v))
   ((string? v) v)
   (else (error 'format-parameter "ANSI parameters must be either strings or numbers; got ~v" v))))

(define-syntax-rule (define-escape-sequence (name arg ...) piece ...)
  (define (name arg ...)
    (let ((arg (format-parameter arg)) ...)
      (string-append piece ...))))

(define-syntax-rule (define-variable-arity-escape-sequence (name args) piece ...)
  (define (name . args)
    (if (null? args)
	(let ((args ""))
	  (string-append piece ...))
	(let ((args (string-append (format-parameter (car args))
				   (foldr (lambda (n acc)
					    (string-append ";" (format-parameter n) acc))
					  ""
					  (cdr args)))))
	  (string-append piece ...)))))

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

(define-variable-arity-escape-sequence (cursor-tabulation-control args) CSI args "W")

(define-escape-sequence (erase-characters n) CSI n "X")

(define-escape-sequence (cursor-line-tabulation n) CSI n "Y")
(define-escape-sequence (cursor-backward-tabulation n) CSI n "Z")

(define-escape-sequence (select-implicit-movement-direction n) CSI n "^")
(define-escape-sequence (character-position-absolute n) CSI n "`")

(define-escape-sequence (character-position-forward n) CSI n "a")
(define-escape-sequence (ansi-repeat n) CSI n "b")

(define-escape-sequence (device-attributes-request n) CSI n "c")
;; See dec-device-attributes-response below.

(define-escape-sequence (line-position-absolute n) CSI n "d")
(define-escape-sequence (line-position-forward n) CSI n "e")
(define-escape-sequence (character-and-line-position row column) CSI row ";" column "f")

(define-escape-sequence (tabulation-clear-character-stop) CSI "0g")
(define-escape-sequence (tabulation-clear-line-stop) CSI "1g")
(define-escape-sequence (tabulation-clear-all-character-stops-on-this-line) CSI "2g")
(define-escape-sequence (tabulation-clear-all-character-stops) CSI "3g")
(define-escape-sequence (tabulation-clear-all-line-stops) CSI "4g")
(define-escape-sequence (tabulation-clear-all-stops) CSI "5g")

(define-variable-arity-escape-sequence (set-mode args) CSI args "h")

(define-escape-sequence (character-position-backward n) CSI n "j")
(define-escape-sequence (line-position-backward n) CSI n "k")

(define-variable-arity-escape-sequence (reset-mode args) CSI args "l")

(define-variable-arity-escape-sequence (select-graphic-rendition parameters) CSI parameters "m")

(define-escape-sequence (device-status-report-reply-ready) CSI "0n")
(define-escape-sequence (device-status-report-reply-busy-retry-later) CSI "1n")
(define-escape-sequence (device-status-report-reply-busy-expect-answer-later) CSI "2n")
(define-escape-sequence (device-status-report-problem-retry-later) CSI "3n")
(define-escape-sequence (device-status-report-problem-expect-answer-later) CSI "4n")
(define-escape-sequence (device-status-report-request) CSI "5n")
(define-escape-sequence (position-report-request) CSI "6n")

(define-variable-arity-escape-sequence (define-area-qualification args) CSI args "o")

(define-escape-sequence (scroll-left n) CSI n " @")
(define-escape-sequence (scroll-right n) CSI n " A")

(define-escape-sequence (page-position-absolute n) CSI n " P")
(define-escape-sequence (page-position-forward n) CSI n " Q")
(define-escape-sequence (page-position-backward n) CSI n " R")

(define-escape-sequence (select-presentation-directions orientation effect)
  CSI orientation ";" effect " S")

(define-escape-sequence (dimension-text-area rows columns) CSI rows ";" columns " T")
(define-escape-sequence (set-line-home n) CSI n " U")
(define-escape-sequence (set-line-limit n) CSI n " V")

(define-escape-sequence (function-key n) CSI n " W")

(define-escape-sequence (tabulation-aligned-trailing-edge n) CSI n " `")
(define-escape-sequence (tabulation-aligned-leading-edge n) CSI n " a")
(define-escape-sequence (tabulation-aligned-centred n) CSI n " b")
(define-escape-sequence (tabulation-aligned-centred-on-character n ch) CSI n ";" ch " c")
(define-escape-sequence (tabulation-stop-remove n) CSI n " d")

;; n = count of 45 degree counterclockwise rotations from normal
;; direction along character path.
(define-escape-sequence (select-character-orientation n) CSI n " e")

(define-escape-sequence (set-page-home n) CSI n " i")
(define-escape-sequence (set-page-limit n) CSI n " j")

(define-escape-sequence (select-character-path-LR/TB effect) CSI "1;" effect " k")
(define-escape-sequence (select-character-path-RL/BT effect) CSI "2;" effect " k")

;; SCO only? (define-escape-sequence (save-cursor-position) CSI "s")
;; SCO only? (define-escape-sequence (restore-cursor-position) CSI "u")

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

(define-escape-sequence (dec-reset-margins) CSI "r")
(define-escape-sequence (dec-set-margins top-row bottom-row) CSI top-row ";" bottom-row "r")

;; dec-set-left-right-margins is apparently for VT400 and printers only:
(define-escape-sequence (dec-set-left-right-margins left right) CSI left ";" right "s")

(define-escape-sequence (dec-soft-terminal-reset) CSI "!p")

(define-variable-arity-escape-sequence (dec-device-attributes-response args) CSI "?" args "c")

;; VT400:
(define-escape-sequence (dec-copy-rectangular-area top left bottom right page
						   target-top target-left target-page)
  CSI top ";" left ";" bottom ";" right ";" page ";" target-top ";" target-left ";" target-page
  "$v")
(define-escape-sequence (dec-fill-rectangular-area char-code top left bottom right page)
  CSI char-code ";" top ";" left ";" bottom ";" right ";" page "$x")
(define-escape-sequence (dec-erase-rectangular-area top left bottom right page)
  CSI top ";" left ";" bottom ";" right ";" page "$z")
(define-escape-sequence (dec-selective-erase-rectangular-area top left bottom right page)
  CSI top ";" left ";" bottom ";" right ";" page "${")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xterm sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-escape-sequence (xterm-full-reset) "\033c") ;; DEC also?

(define (xterm-set-icon-name-and-window-title text)
  (string-append OSC "0;" text ST))
(define (xterm-set-icon-name text)
  (string-append OSC "1;" text ST))
(define (xterm-set-window-title text)
  (string-append OSC "2;" text ST))

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
;; Parameters for define-area-qualification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define qualified-area-unprotected-and-unguarded 0)
(define qualified-area-protected-and-guarded 1)
(define qualified-area-graphic-input 2)
(define qualified-area-numeric-input 3)
(define qualified-area-alphabetic-input 4)
(define qualified-area-right-aligned-input 5)
(define qualified-area-zero-fill 6)
(define qualified-area-tab-stop-field-start 7) ;; no idea what this does, sorry about the name
(define qualified-area-protected-and-unguarded 8)
(define qualified-area-space-fill 9)
(define qualified-area-left-aligned-input 10)
(define qualified-area-reversed-order 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters for select-presentation-directions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; direction-LINEORIENTATION-CHARACTERPATH-LINEPROGRESSION
(define direction-horizontal-LR-TB 0)
(define direction-vertical-TB-RL 1)
(define direction-vertical-TB-LR 2)
(define direction-horizontal-RL-TB 3)
(define direction-vertical-BT-LR 4)
(define direction-horizontal-RL-BT 5)
(define direction-horizontal-LR-BT 6)
(define direction-vertical-BT-RL 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters for select-presentation-directions,
;; select-character-path-LR/TB, and select-character-path-RL/BT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define effect-undefined 0)
(define effect-update-presentation-from-data 1)
(define effect-update-data-from-presentation 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes (for set-mode and reset-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note that for some of the non-standard modes, the codes
;; conflict. See, for example, "?9", which controls interlace-mode for
;; DEC and X10 mouse-reporting for xterms.

;; Standard modes

(define guarded-area-transfer-mode "1")
(define keyboard-action-mode "2")
(define control-representation-mode "3")
(define insertion-replacement-mode "4")
(define status-report-transfer-mode "5")
(define erasure-mode "6")
(define line-editing-mode "7")
(define bi-directional-support-mode "8")
(define device-component-select-mode "9")
(define character-editing-mode "10")
(define positioning-unit-mode "11") ;; deprecated
(define send/receive-mode "12")
(define format-effector-action-mode "13")
(define format-effector-transfer-mode "14")
(define multiple-area-transfer-mode "15")
(define transfer-termination-mode "16")
(define selected-area-transfer-mode "17")
(define tabulation-stop-mode "18")
(define editing-boundary-mode "19") ;; obsolete
(define line-feed/new-line-mode "20") ;; obsolete
(define graphic-rendition-combination-mode "21")
(define zero-default-mode "22") ;; deprecated

;; dec
(define ansi/vt52-mode "?2")
(define column-mode "?3")
(define scrolling-mode "?4")
(define screen-mode "?5")
(define origin-mode "?6")
(define autowrap-mode "?7")
(define auto-repeat-mode "?8")
(define interlace-mode "?9")
(define editing-mode "?10")
(define line-transmit-mode "?11")
(define space-compression/field-delimiter-mode "?13")
(define transmit-execution-mode "?14")
(define edit-key-execution-mode "?16")
(define print-form-feed-mode "?18")
(define printer-extent-mode "?19")
(define print-density-mode "?24")
(define text-cursor-enable-mode "?25")
(define proportional-spacing-mode "?27")
(define pitch-select-mode "?29")
(define right-to-left-writing-direction-mode "?34")
(define hebrew-encoding-mode "?36")
(define tek-graphics-mode "?38")
(define carriage-return/new-line-mode "?40")
(define non-bidirectional-print-direction-mode "?41")
(define nat-repl-char "?42") ;; no idea what this might mean
(define expanded/compressed-print-mode "?43")
(define print-color/black-and-white-mode "?44")
(define rgb/hls-print-color-syntax-mode "?45")
(define graphics-print-background-mode "?46")
(define print-rotated/compressed-mode "?47")
(define black/white-reversal-mode "?51")
(define origin-placement-mode "?52")
(define bold-page-mode "?55")
(define horizontal-cursor-coupling-mode "?60")
(define vertical-cursor-coupling-mode "?61")
(define page-cursor-coupling-mode "?64")
(define numeric-keypad-mode "?66")
(define backspace/delete-mode "?67")
(define typewriter-mode "?68")
(define sixel-scrolling-mode "?80")

;; xterm
(define x10-mouse-reporting-mode "?9")
(define column-switch-enabled-mode "?40")
(define more-fix-mode "?41")
(define margin-bell-mode "?44")
(define reverse-wraparound-mode "?45")
(define logging-mode "?46")
(define alternate-screen-mode "?47")
(define x11-normal-mouse-tracking-mode "?1000")
(define x11-hilite-mouse-tracking-mode "?1001")
(define x11-button-event-mouse-tracking-mode "?1002")
(define x11-any-event-mouse-tracking-mode "?1003")
(define alternate-screen-buffer-mode "?1047")
(define save/restore-cursor-pseudomode "?1048")
(define save/restore-cursor-and-alternate-screen-buffer-pseudomode "?1049")
(define sun-function-key-mode "?1051")
(define hp-function-key-mode "?1052")
(define sun/pc-keyboard-mode "?1061")

;; rxvt (presumably supported also by (modern) xterms?)
(define scroll-bar-mode "?30")
(define shifted-key-functions-mode "?35")
(define scroll-to-bottom-on-tty-output-mode "?1010")
(define scroll-to-bottom-on-key-press-mode "?1011")
(define special-modifiers-mode "?1035")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derived sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-escape-sequence (kill-line)
  (goto-column 1)
  (clear-to-eol))

(define-escape-sequence (clear-screen/home)
  (clear-screen)
  (goto 1 1))
