#lang racket/base
;; Keyspecs, keyseqs and keymaps

(provide parse-key-sequence
         keyspec->keyseq
         key->keyspec
         keyseq->keyspec
         (struct-out keymap)
         empty-keymap
         keymap-update
         keymap-bind
         keymap-unbind
         keymap-lookup)

(require racket/set)
(require racket/match)
(require (only-in racket/list append-map))
(require (only-in racket/string
                  string-join
                  string-split
                  string-trim))

(require ansi/lcd-terminal)

;;---------------------------------------------------------------------------
;; Key sequence parsing

(define (read-string-to-end s)
  (define p (open-input-string s))
  (define result (read p))
  (and (eof-object? (peek-char p))
       result))

(define (bad-key lexeme fmt . args)
  (error 'parse-key-sequence "~a in key ~v" (apply format fmt args) (string-trim lexeme)))

(define (parse-modifiers modifiers lexeme)
  (for/set ((mod (string-split (string-upcase modifiers) "-")))
    (match mod
      ["C" 'control]
      ["S" 'shift]
      ["M" 'meta]
      [_ (bad-key lexeme "Unknown modifier ~a" mod)])))

(define (parse-key-sequence s)
  (match s
    [(pregexp "^ *#:default(( +.*)|$)" (list lexeme rest _))
     (cons '#:default (parse-key-sequence rest))]
    [(pregexp "^ *(([cCsSmM]-)*)\"([^\"]*)\"(.*)" (list lexeme modifiers _ stringspec rest))
     (define mods (parse-modifiers modifiers lexeme))
     (define seq (unknown-escape-sequence (or (read-string-to-end (format "\"~a\"" stringspec))
                                              (bad-key lexeme "Bad raw input sequence"))))
     (cons (key seq mods) (parse-key-sequence rest))]
    [(pregexp "^ *(([cCsSmM]-)*)<([^>]+)>(( +.*)|$)" (list lexeme modifiers _ symname rest _))
     (define mods (parse-modifiers modifiers lexeme))
     (cons (key (string->symbol symname) mods)
           (parse-key-sequence rest))]
    [(pregexp "^ *(([cCsSmM]-)*)(?i:esc)(( +.*)|$)" (list lexeme modifiers _ rest _))
     (define mods (parse-modifiers modifiers lexeme))
     (cons (key #\[ (set-add mods 'control)) (parse-key-sequence rest))]
    [(pregexp "^ *(([cCsSmM]-)*)([^ ]+)(( +.*)|$)" (list lexeme modifiers _ keystr rest _))
     (define mods (parse-modifiers modifiers lexeme))
     (define keychar (or (read-string-to-end (format "#\\~a" keystr))
                         (bad-key lexeme "Bad single-character key")))
     (cons (key (if (set-member? mods 'control)
                    (char-upcase keychar)
                    keychar)
                mods)
           (parse-key-sequence rest))]
    [(pregexp "^ *$")
     '()]
    [_ (bad-key s "Unexpected junk")]))

(define (keyspec->keyseq what original-keyspec)
  (let convert ((keyspec original-keyspec))
    (cond
     [(key? keyspec) (list keyspec)]
     [(keyword? keyspec) (list keyspec)]
     [(string? keyspec) (parse-key-sequence keyspec)]
     [(list? keyspec) (append-map convert keyspec)]
     [else (error what "Invalid key specification: ~v" original-keyspec)])))

(define (format-modifiers mods suffix)
  (if (set-empty? mods)
      suffix
      (string-append (string-join (map (lambda (m)
                                         (match m
                                           ['control "C"]
                                           ['shift "S"]
                                           ['meta "M"]))
                                       (set->list mods))
                                  "-")
                     "-"
                     suffix)))

(define (key->keyspec k)
  (match k
    [(? keyword?) (format "~a" k)]
    [(key value modifiers)
     (define-values (str updated-modifiers)
       (match value
         [(unknown-escape-sequence s)
          (values (format "~v" s) modifiers)]
         [(? symbol? s)
          (values (format "<~a>" s) modifiers)]
         [#\[ #:when (set-member? modifiers 'control)
          (values "ESC" (set-remove modifiers 'control))]
         [(? char? c)
          (define s (format "~v" c))
          (values (substring s 2 (string-length s)) modifiers)]))
     (format-modifiers updated-modifiers str)]))

(define (keyseq->keyspec keyseq)
  (string-join (map key->keyspec keyseq) " "))

;;---------------------------------------------------------------------------
;; Keymaps

(struct keymap (table
                ) #:prefab)

(define (empty-keymap)
  (keymap (hash)))

(define (keymap-update km keyspec updater)
  (define original-keyseq (keyspec->keyseq 'keymap-bind keyspec))
  (let loop ((prefix-rev '())
             (keyseq original-keyseq)
             (km km))
    (match keyseq
      ['() (updater (reverse prefix-rev) km original-keyseq)]
      [(cons k rest)
       (cond
        [(keymap? km)
         (let* ((new (loop (cons k prefix-rev) rest (hash-ref (keymap-table km) k #f)))
                (newtab (if new
                            (hash-set (keymap-table km) k new)
                            (hash-remove (keymap-table km) k))))
           (if (hash-empty? newtab)
               #f
               (struct-copy keymap km [table newtab])))]
        [(not km)
         (loop prefix-rev keyseq (empty-keymap))]
        [else
         (error 'keymap-update
                "Cannot update keyspec ~v, as a shorter prefix ~v exists"
                (keyseq->keyspec original-keyseq)
                (keyseq->keyspec (reverse prefix-rev)))])])))

(define (keymap-bind km keyspec command)
  (keymap-update km keyspec (lambda (prefix oldval newseq)
                              (if oldval
                                  (error 'keymap-bind "Cannot bind ~v, as prefix ~v exists"
                                         (keyseq->keyspec newseq)
                                         (keyseq->keyspec prefix))
                                  command))))

(define (keymap-bind* km specs-and-commands)
  (match specs-and-commands
    ['() km]
    [(cons (list keyspec command) rest) (keymap-bind* (keymap-bind km keyspec command) rest)]))

(define (keymap-unbind km keyspec)
  (or (keymap-update km keyspec (lambda (prefix oldval newseq) #f))
      (empty-keymap)))

(define (keymap-lookup km keyspec)
  (define original-keyseq (keyspec->keyseq 'keymap-lookup keyspec))
  (let loop ((keyseq original-keyseq)
             (km km))
    (match keyseq
      ['() (values km keyseq)]
      [(cons k rest)
       (match km
         [(keymap table) (loop rest (or (hash-ref table k #f)
                                        (hash-ref table '#:default #f)))]
         [_ (values km keyseq)])])))

;;---------------------------------------------------------------------------

(module+ test
  (require rackunit racket/pretty)

  (check-equal? (parse-key-sequence "<") (list (key #\< (set))))
  (check-equal? (parse-key-sequence ">") (list (key #\> (set))))
  (check-equal? (parse-key-sequence "#:default #:default")
                (list '#:default '#:default))
  (check-equal? (parse-key-sequence "esc ESC")
                (list (key #\[ (set 'control))
                      (key #\[ (set 'control))))

  (define km (keymap-bind* (empty-keymap) (list (list "C-x o" 'other-window)
                                                (list "C-x 2" 'split-window)
                                                (list "C-x 1" 'delete-other-windows)
                                                (list "C-x 0" 'delete-window))))
  (check-equal? km
                (keymap (hash (key #\X (set 'control))
                              (keymap (hash (key #\o (set)) 'other-window
                                            (key #\2 (set)) 'split-window
                                            (key #\1 (set)) 'delete-other-windows
                                            (key #\0 (set)) 'delete-window)))))
  (set! km (keymap-unbind km "C-x 1"))
  (check-equal? km
                (keymap (hash (key #\X (set 'control))
                              (keymap (hash (key #\o (set)) 'other-window
                                            (key #\2 (set)) 'split-window
                                            (key #\0 (set)) 'delete-window)))))
  (check-equal? (keymap-unbind (keymap-unbind km "C-x 2") "C-x 0")
                (keymap (hash (key #\X (set 'control))
                              (keymap (hash (key #\o (set)) 'other-window)))))
  (check-equal? (keymap-unbind (keymap-unbind (keymap-unbind km "C-x 2") "C-x 0") "C-x o")
                (empty-keymap))
  (check-equal? (keymap-unbind km "C-x")
                (empty-keymap))

  (define (lookup s)
    (define-values (result remaining-input) (keymap-lookup km s))
    (list result remaining-input))

  (check-equal? (lookup "C-x") (list (keymap (hash (key #\o (set)) 'other-window
                                                   (key #\2 (set)) 'split-window
                                                   (key #\0 (set)) 'delete-window))
                                     '()))
  (check-equal? (lookup "C-x 1") (list #f '()))
  (check-equal? (lookup "C-x 2") (list 'split-window '()))
  (check-equal? (lookup "C-c") (list #f '()))
  (check-equal? (lookup "C-c C-c") (list #f (list (key #\C (set 'control)))))
  )
