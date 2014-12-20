#lang racket/base
;; Lowest Common Denominator terminal.

(provide (struct-out key)
         (struct-out unknown-escape-sequence)
         (struct-out position-report)
         lex-lcd-input)

(require racket/set)
(require racket/match)
(require (only-in racket/string string-split))

(define lcd-terminal-utf-8? (make-parameter #t))

(struct unknown-escape-sequence (string) #:prefab)
(struct key (value modifiers) #:prefab)
(struct position-report (row column) #:prefab)

(define (simple-key value) (key value (set)))
(define (S- value) (key value (set 'shift)))
(define (C- value) (key value (set 'control)))
(define (M- value) (key value (set 'meta)))
(define (C-S- value) (key value (set 'control 'shift)))
(define (C-M- value) (key value (set 'control 'meta)))

(define (add-modifier modifier k)
  (struct-copy key k [modifiers (set-add (key-modifiers k) modifier)]))

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
                            [11 (ctor 'f1)] [12 (ctor 'f2)] [13 (ctor 'f3)] [14 (ctor 'f4)]
                            [15 (ctor 'f5)] [17 (ctor 'f6)] [18 (ctor 'f7)] [19 (ctor 'f8)]
                            [20 (ctor 'f9)] [21 (ctor 'f10)] [23 (ctor 'f11)] [24 (ctor 'f12)]
                            [25 (ctor 'f13)] [26 (ctor 'f14)] [28 (ctor 'f15)] [29 (ctor 'f16)]
                            [31 (ctor 'f17)] [32 (ctor 'f18)] [33 (ctor 'f19)] [34 (ctor 'f20)]
                            [_ (simple-key (unknown-escape-sequence lexeme))])))

(define (analyze-vt-bracket-key lexeme params mainchar)
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
    ["J" #:when (equal? params '(2)) (S- 'home)] ;; st, http://st.suckless.org/
    ["J" #:when (not params) (C- 'end)] ;; st, http://st.suckless.org/
    ["K" #:when (equal? params '(2)) (S- 'delete)] ;; st, http://st.suckless.org/
    ["K" #:when (not params) (S- 'end)] ;; st, http://st.suckless.org/
    ["L" (C- 'insert)]                  ;; st, http://st.suckless.org/
    ["M" (C- 'delete)]                  ;; st, http://st.suckless.org/
    ["P" #:when (not params) (simple-key 'delete)] ;; st, http://st.suckless.org/
    ["P" (decode-shifting params 'f1)]
    ["Q" (decode-shifting params 'f2)]
    ["R" #:when (and (= (length params) 2) (> (car params) 1)) (apply position-report params)]
    ["R" (decode-shifting params 'f3)]
    ["S" (decode-shifting params 'f4)]
    ["Z" (C-S- #\I)] ;; TODO: should this instead be a 'backtab key?
    ["a" (S- 'up)]
    ["b" (S- 'down)]
    ["c" (S- 'right)]
    ["d" (S- 'left)]
    ["h" #:when (equal? params '(4)) (simple-key 'insert)] ;; st, http://st.suckless.org/
    [_ (simple-key (unknown-escape-sequence lexeme))]))

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
    ["P" (simple-key 'f1)]
    ["Q" (simple-key 'f2)]
    ["R" (simple-key 'f3)]
    ["S" (simple-key 'f4)]
    [_ (simple-key (unknown-escape-sequence lexeme))]))

(define (interpret-ascii-code b)
  (cond
   [(<= #x00 b #x1f) (C- (integer->char (+ b (char->integer #\A) -1)))]
   [(<= #x20 b #x7e) (simple-key (integer->char b))]
   [(= b #x7f) (simple-key 'backspace)]))

(define (lex-lcd-input port)
  (cond
   [(eof-object? (peek-byte port)) eof]
   [(or (regexp-try-match #px"^\e\\[([0-9]+(;[0-9]+)*)?(.)" port)
        (regexp-try-match #px#"^\x9b([0-9]+(;[0-9]+)*)?(.)" port)) =>
    (lambda (match-result)
      (match-define (list lexeme parambytes _ mainbytes) match-result)
      (define params (and parambytes
                          (map string->number (string-split (bytes->string/utf-8 parambytes) ";"))))
      (analyze-vt-bracket-key lexeme params (bytes->string/utf-8 mainbytes)))]
   [(regexp-try-match #px"^\eO([0-9])(.)" port) =>
    ;; screen generates shifting escapes for the keypad like this
    (lambda (match-result)
      (match-define (list lexeme v-plus-one-bytes mainbytes) match-result)
      (decode-shifting-number (string->number (bytes->string/utf-8 v-plus-one-bytes))
                              (analyze-vt-O-mainchar lexeme (bytes->string/utf-8 mainbytes))))]
   [(regexp-try-match #px"^\eO(.)" port) =>
    (lambda (match-result)
      (match-define (list lexeme mainbytes) match-result)
      (analyze-vt-O-mainchar lexeme (bytes->string/utf-8 mainbytes)))]
   ;; Characters between #\u80 and #\uff are ambiguous because in
   ;; some terminals, the high bit is set to indicate meta, and in
   ;; others, they are plain UTF-8 characters. We let the user
   ;; distinguish via the lcd-terminal-utf-8? parameter.
   [(not (lcd-terminal-utf-8?))
    (define b (read-byte port))
    (if (< b 128)
        (interpret-ascii-code b)
        (add-modifier 'meta (interpret-ascii-code (- b 128))))]
   [else
    (define b (char->integer (read-char port)))
    (if (< b 128)
        (interpret-ascii-code b)
        (simple-key (integer->char b)))]))
