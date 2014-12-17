#lang racket/base
;; Lowest Common Denominator terminal.

(provide (struct-out key)
         (struct-out unknown-escape-sequence)
         lex-lcd-input)

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require racket/set)
(require racket/match)
(require (only-in racket/string string-split))

(struct unknown-escape-sequence (string) #:prefab)
(struct key (value modifiers) #:prefab)

(define (simple-key value) (key value (set)))
(define (S- value) (key value (set 'shift)))
(define (C- value) (key value (set 'control)))
(define (M- value) (key value (set 'meta)))
(define (C-S- value) (key value (set 'control 'shift)))
(define (C-M- value) (key value (set 'control 'meta)))

(define (add-modifier modifier k)
  (struct-copy key k [modifiers (set-add (key-modifiers k) modifier)]))

(define (char+ c offset)
  (integer->char (+ (char->integer c) offset)))

(define (control-character lexeme offset ctor)
  (ctor (char+ (string-ref lexeme 0) (+ (char->integer #\A) -1 offset))))

(define (decode-shifting-number v-plus-one k)
  (define v (- v-plus-one 1))
  (let* ((k (if (zero? (bitwise-and v 1)) k (add-modifier 'shift k)))
         (k (if (zero? (bitwise-and v 2)) k (add-modifier 'meta k)))
         (k (if (zero? (bitwise-and v 4)) k (add-modifier 'control k))))
    k))

(define (decode-shifting params value)
  (match params
    [(list 1 v-plus-one) (decode-shifting-number v-plus-one (simple-key value))]
    [_ (simple-key value)] ;; bit of a cop-out
    ))

(define (analyze-vt-tildeish-key lexeme params ctor)
  (match params
    [(list a b) (analyze-vt-tildeish-key* lexeme ctor a b)]
    [(list a) (analyze-vt-tildeish-key* lexeme ctor a 1)]
    [_ (simple-key (unknown-escape-sequence lexeme))]))

(define (analyze-vt-tildeish-key* lexeme ctor a b)
  (decode-shifting-number b
                          (match a
                            [1 (ctor 'home)] ;; linux console
                            [2 (ctor 'insert)]
                            [3 (ctor 'delete)]
                            [4 (ctor 'end)] ;; linux console
                            [5 (ctor 'page-up)]
                            [6 (ctor 'page-down)]
                            [7 (ctor 'home)]
                            [8 (ctor 'end)]
                            [_ (simple-key (unknown-escape-sequence lexeme))])))

(define (analyze-vt-bracket-key lexeme)
  (match lexeme
    [(pregexp "\e\\[([0-9]+(;[0-9]+)*)?(.)" (list _ paramstr _ mainchar))
     (define params (and paramstr (map string->number (string-split paramstr ";"))))
     (match mainchar
       ["~" (analyze-vt-tildeish-key lexeme params simple-key)]
       ["$" (analyze-vt-tildeish-key lexeme params S-)]
       ["^" (analyze-vt-tildeish-key lexeme params C-)]
       ["@" (analyze-vt-tildeish-key lexeme params C-S-)]
       ["A" (decode-shifting params 'up)]
       ["B" (decode-shifting params 'down)]
       ["C" (decode-shifting params 'right)]
       ["D" (decode-shifting params 'left)]
       ["E" (decode-shifting params 'begin)]
       ["F" (decode-shifting params 'end)]
       ["G" (decode-shifting params 'begin)] ;; linux console (!)
       ["H" (decode-shifting params 'home)]
       ["Z" (C-S- #\I)] ;; TODO: should this instead be a 'backtab key?
       ["a" (S- 'up)]
       ["b" (S- 'down)]
       ["c" (S- 'right)]
       ["d" (S- 'left)]
       [_ (simple-key (unknown-escape-sequence lexeme))])]
    [_ (error 'analyze-vt-bracket-key "Unexpected input sequence from lexer: ~v" lexeme)]))

(define (analyze-vt-O-mainchar lexeme mainchar)
  (match mainchar
    ["a" (C- 'up)]
    ["b" (C- 'down)]
    ["c" (C- 'right)]
    ["d" (C- 'left)]

    ;; rxvt keypad keys.
    ;; Per http://www.vt100.net/docs/vt102-ug/appendixc.html, these
    ;; are "ANSI Alternate Keypad Mode" sequences.
    ["j" (simple-key #\*)]
    ["k" (simple-key #\+)]
    ["l" (simple-key #\,)] ;; my keypad doesn't have a comma
    ["m" (simple-key #\-)]
    ["n" (simple-key 'delete)] ;; #\.
    ["o" (simple-key #\/)]
    ["p" (simple-key 'insert)]       ;; #\0
    ["q" (simple-key 'end)]          ;; #\1
    ["r" (simple-key 'down)]         ;; #\2
    ["s" (simple-key 'page-down)]    ;; #\3
    ["t" (simple-key 'left)]         ;; #\4
    ["u" (simple-key 'begin)]        ;; #\5
    ["v" (simple-key 'right)]        ;; #\6
    ["w" (simple-key 'home)]         ;; #\7
    ["x" (simple-key 'up)]           ;; #\8
    ["y" (simple-key 'page-up)]      ;; #\9

    ["A" (simple-key 'up)]                            ;; kcuu1
    ["B" (simple-key 'down)]                          ;; kcud1
    ["C" (simple-key 'right)]                         ;; kcuf1
    ["D" (simple-key 'left)]                          ;; kcub1
    ["E" (simple-key 'begin)]                         ;; in screen
    ["F" (simple-key 'end)]                           ;; kend
    ["H" (simple-key 'home)]                          ;; khome
    ["M" (add-modifier 'control (simple-key #\M))] ;; keypad enter (rxvt)
    [_ (simple-key (unknown-escape-sequence lexeme))]))

(define (analyze-vt-O-key lexeme)
  (match lexeme
    [(pregexp "\eO(.)(.)" (list _ v-plus-one-str mainchar))
     ;; screen generates shifting escapes for the keypad like this
     (decode-shifting-number (string->number v-plus-one-str)
                             (analyze-vt-O-mainchar lexeme mainchar))]
    [(pregexp "\eO(.)" (list _ mainchar))
     (analyze-vt-O-mainchar lexeme mainchar)]
    [other (simple-key (unknown-escape-sequence lexeme))]))

(define lex-lcd-input
  (lexer [(eof) eof]
         [(char-range #\u00 #\u1f) (control-character lexeme 0 C-)]
         [(char-range #\u20 #\u7e) (simple-key (string-ref lexeme 0))]
         [#\u7f (simple-key 'backspace)]
         [(char-range #\u80 #\u9f) (control-character lexeme -128 C-M-)]
         [(char-range #\u80 #\ufe) (M- (char+ (string-ref lexeme 0) -128))]
         [#\uff (M- 'backspace)]
         [(:: "\e[" (:? (:+ numeric) (:* #\; (:+ numeric))) any-char)
          (analyze-vt-bracket-key lexeme)]
         [(:: "\eO" any-char) (analyze-vt-O-key lexeme)]
         [(:: "\eO" numeric any-char) (analyze-vt-O-key lexeme)]
         ))

(module+ main
  (require "tty-raw-extension")
  (tty-raw!)
  (let loop ()
    (match (lex-lcd-input (current-input-port))
      [(? eof-object?) (void)]
      [(== (C- #\D)) (void)]
      [key
       (printf "Key: ~v\r\n" key)
       (loop)]))
  (tty-restore!))
