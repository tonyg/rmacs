#lang racket/base

(provide (struct-out tty)
         (struct-out pen)
         stdin-tty
         tty-rows
         tty-columns
         tty-last-row
         tty-last-column
         tty-cursor-row
         tty-cursor-column
         tty-display
         tty-newline
         tty-clear
         tty-clear-to-eol
         tty-reset
         tty-goto
         tty-set-pen!
         tty-default-pen
         tty-pen
         tty-flush
         tty-next-key
         tty-next-key-evt

         ;; From ansi
         (rename-out [ansi:color-black color-black]
                     [ansi:color-red color-red]
                     [ansi:color-green color-green]
                     [ansi:color-yellow color-yellow]
                     [ansi:color-blue color-blue]
                     [ansi:color-magenta color-magenta]
                     [ansi:color-cyan color-cyan]
                     [ansi:color-white color-white]))

(require racket/set)
(require racket/match)
(require (only-in racket/vector vector-copy))
(require (prefix-in ansi: ansi))

(require "diff.rkt")

(struct pen (foreground-color ;; Nat
             background-color ;; Nat
             bold? ;; Boolean
             italic? ;; Boolean
             ) #:prefab)

(struct screen (rows ;; Nat
                columns ;; Nat
                [cursor-row #:mutable] ;; Nat
                [cursor-column #:mutable] ;; Nat
                [pen #:mutable] ;; Pen
                contents ;; (Vector[rows] (Vector[columns] (Cons Pen Character)))
                ) #:prefab)

(struct tty (input ;; InputPort
             output ;; OutputPort
             key-reader ;; InputPort -> Key
             [displayed-screen #:mutable] ;; Screen
             [pending-screen #:mutable] ;; Screen
             [utf-8-input? #:mutable] ;; Boolean
             ) #:prefab)

(define (make-screen rows columns pen)
  (define contents (for/vector ((row rows)) (make-vector columns (cons pen 'empty))))
  (screen rows columns 0 0 pen contents))

(define (copy-screen s)
  (match-define (screen rows columns cursor-row cursor-column pen contents) s)
  (define new-contents (for/vector ((row rows)) (vector-copy (vector-ref contents row))))
  (screen rows columns cursor-row cursor-column pen new-contents))

(define tty-default-pen 'default)

(define *stdin-tty* #f)
(define (stdin-tty)
  (when (not *stdin-tty*)
    (ansi:tty-raw!)
    (set! *stdin-tty*
          (tty (current-input-port)
               (current-output-port)
               ansi:lex-lcd-input
               (make-screen 24 80 tty-default-pen)
               (make-screen 24 80 tty-default-pen)
               (match (getenv "RMACS_UTF8_INPUT")
                 [(or #f "yes" "true" "1") #t]
                 [(or "no" "false" "0") #f]
                 [v (error 'RMACS_UTF8_INPUT
                           "Environment variable RMACS_UTF8_INPUT value ~v invalid: must be in ~v"
                           v
                           (list "yes" "true" "1" "no" "false" "0"))])))
    (reset *stdin-tty*)
    (plumber-add-flush! (current-plumber)
                        (lambda (h)
                          (output *stdin-tty*
                                  (ansi:select-graphic-rendition ansi:style-normal)
                                  (ansi:goto (tty-rows *stdin-tty*) 1))
                          (flush *stdin-tty*))))
  *stdin-tty*)

;;---------------------------------------------------------------------------
;; Actually send changes to the display

(define (collect-position-report tty)
  (let loop ()
    (sync/timeout 0.5
                  (handle-evt (tty-input tty)
                              (lambda (p)
                                (match ((tty-key-reader tty) p)
                                  [(? ansi:position-report? r) r]
                                  [_ (loop)]))))))

(define (reset tty)
  (output tty
          (ansi:clear-screen)
          (ansi:goto 999 999)
          (ansi:position-report-request))
  (flush tty)
  (define report (or (collect-position-report tty)
                     (ansi:position-report 24 80))) ;; TODO: have a more flexible fallback
  ;; (set! report (ansi:position-report 5 10))
  (define rows (ansi:position-report-row report))
  (define columns (ansi:position-report-column report))
  (set-pen tty tty-default-pen #:force #t)
  (clear tty)
  (flush tty)
  (set-tty-displayed-screen! tty (make-screen rows columns tty-default-pen))
  (set-tty-pending-screen! tty (make-screen rows columns tty-default-pen))
  tty)

(define (set-pen tty p #:force [force #f])
  (when (or force (not (equal? p (screen-pen (tty-displayed-screen tty)))))
    (match p
      [(pen fgcolor bgcolor bold? italic?)
       (output tty
               (apply ansi:select-graphic-rendition
                      `(,@(if bold? (list ansi:style-bold) (list))
                        ,@(if italic? (list ansi:style-italic/inverse) (list))
                        ,(ansi:style-text-color fgcolor)
                        ,(ansi:style-background-color bgcolor))))]
      ['default
       (output tty (ansi:select-graphic-rendition ansi:style-normal))])
    (set-screen-pen! (tty-displayed-screen tty) p))
  tty)

(define (clear tty)
  (output tty (ansi:clear-screen/home))
  (set-screen-cursor-row! (tty-displayed-screen tty) 0)
  (set-screen-cursor-column! (tty-displayed-screen tty) 0)
  tty)

(define (color-near-cursor s row-delta column-delta)
  (define r (max 0 (min (- (screen-rows s) 1) (+ (screen-cursor-row s) row-delta))))
  (define c (max 0 (min (- (screen-columns s) 1) (+ (screen-cursor-column s) column-delta))))
  (car (vector-ref (vector-ref (screen-contents s) r) c)))

(define (vector-delete! v base count fill)
  (vector-copy! v base v (+ base count) (vector-length v))
  (for ((i (in-range (- (vector-length v) count) (vector-length v)))) (vector-set! v i fill)))

(define (vector-insert! v base count fill)
  (vector-copy! v (+ base count) v base (- (vector-length v) count))
  (for ((i (in-range base (+ base count)))) (vector-set! v i fill)))

(define (delete-lines tty n)
  (define s (tty-displayed-screen tty))
  (set-pen tty tty-default-pen)
  (output tty (ansi:delete-lines n))
  (define blank-line (make-vector (screen-columns s) (cons (screen-pen s) 'empty)))
  (vector-delete! (screen-contents s) (screen-cursor-row s) n blank-line)
  tty)

(define (insert-lines tty n)
  (define s (tty-displayed-screen tty))
  (set-pen tty tty-default-pen)
  (output tty (ansi:insert-lines n))
  (define blank-line (make-vector (screen-columns s) (cons (screen-pen s) 'empty)))
  (vector-insert! (screen-contents s) (screen-cursor-row s) n blank-line)
  tty)

(define (delete-columns tty n)
  (define s (tty-displayed-screen tty))
  (set-pen tty tty-default-pen)
  (output tty (ansi:delete-characters n))
  (define blank-cell (cons (screen-pen s) 'empty))
  (define line (vector-ref (screen-contents s) (screen-cursor-row s)))
  (vector-delete! line (screen-cursor-column s) n blank-cell)
  tty)

(define (insert-columns tty n)
  (define s (tty-displayed-screen tty))
  (set-pen tty (color-near-cursor s 0 -1))
  (output tty (ansi:insert-characters n))
  (define blank-cell (cons (screen-pen s) 'empty))
  (define line (vector-ref (screen-contents s) (screen-cursor-row s)))
  (vector-insert! line (screen-cursor-column s) n blank-cell)
  tty)

(define (output tty . items)
  (for ((i items)) (display i (tty-output tty))))

(define (flush tty)
  (flush-output (tty-output tty)))

;;---------------------------------------------------------------------------
;; Display to buffered screen

(define (tty-rows t) (screen-rows (tty-pending-screen t)))
(define (tty-columns t) (screen-columns (tty-pending-screen t)))

(define (tty-last-row t) (- (tty-rows t) 1))
(define (tty-last-column t) (- (tty-columns t) 1))

(define (tty-cursor-row t) (screen-cursor-row (tty-pending-screen t)))
(define (tty-cursor-column t) (screen-cursor-column (tty-pending-screen t)))

(define (non-empty? ch) (not (equal? ch 'empty)))

(define (putc tty ch)
  (define s (tty-pending-screen tty))
  (match ch
    [#\return
     (tty-goto tty (screen-cursor-row s) 0)]
    [#\newline
     (tty-goto tty (+ (screen-cursor-row s) 1) (screen-cursor-column s))]
    [#\tab
     (for ((i (- 8 (modulo (screen-cursor-column s) 8)))) (putc tty #\space))]
    [(and (? non-empty?) (? char-iso-control?))
     (puts tty (format "[~x]" (char->integer ch)))]
    [_
     (when (< (screen-cursor-column s) (screen-columns s))
       ;; (tty-goto tty (+ (screen-cursor-row s) 1) 0)
       (vector-set! (vector-ref (screen-contents s) (screen-cursor-row s))
                    (screen-cursor-column s)
                    (cons (screen-pen s) ch)))
     (set-screen-cursor-column! s (+ (screen-cursor-column s) 1))]))

(define (puts tty s)
  (for ((ch s)) (putc tty ch)))

(define (tty-display tty . strings)
  (for ((s strings)) (puts tty s)))

(define (tty-newline tty)
  (tty-clear-to-eol tty)
  (putc tty #\return)
  (putc tty #\newline))

(define (tty-clear tty)
  (set-tty-pending-screen! tty (make-screen (tty-rows tty) (tty-columns tty) (tty-pen tty)))
  tty)

(define (tty-clear-to-eol tty)
  (define start-column (tty-cursor-column tty))
  (define pen (screen-pen (tty-pending-screen tty)))
  (tty-set-pen! tty tty-default-pen)
  (for ((i (max 0 (- (tty-columns tty) (tty-cursor-column tty))))) (putc tty 'empty))
  (tty-set-pen! tty pen)
  (tty-goto tty (tty-cursor-row tty) start-column)
  tty)

(define (tty-reset tty)
  (reset tty)
  tty)

(define (tty-goto tty row0 column0)
  (define row (max 0 (min (tty-last-row tty) row0)))
  (define column (max 0 (min (tty-last-column tty) column0)))
  (set-screen-cursor-row! (tty-pending-screen tty) row)
  (set-screen-cursor-column! (tty-pending-screen tty) column)
  tty)

(define (tty-set-pen! tty pen)
  (set-screen-pen! (tty-pending-screen tty) pen)
  tty)

(define (tty-pen tty)
  (screen-pen (tty-pending-screen tty)))

;; (define (dump-screen s)
;;   (list 'screen
;;         (screen-rows s)
;;         (screen-columns s)
;;         (screen-cursor-row s)
;;         (screen-cursor-column s)
;;         (list->string
;;          (for*/list ((line (screen-contents s))
;;                      (cell line)
;;                      #:when (non-empty? (cdr cell)))
;;            (cdr cell)))))

(define (goto-if-needed s row column)
  (cond
   [(and (= (screen-cursor-row s) row) (= (screen-cursor-column s) column))
    ""]
   [(= (screen-cursor-row s) row)
    (begin0 (ansi:goto-column (+ column 1))
      (set-screen-cursor-column! s column))]
   [else
    (begin0 (ansi:goto (+ row 1) (+ column 1))
      (set-screen-cursor-row! s row)
      (set-screen-cursor-column! s column))]))

(define (advance-cursor! tty s)
  (set-screen-cursor-column! s (+ (screen-cursor-column s) 1))
  (when (= (screen-cursor-column s) (screen-columns s))
    (when (< (screen-cursor-row s) (- (screen-rows s) 1))
      (output tty "\r\n"))
    (set-screen-cursor-column! s 0)
    (set-screen-cursor-row! s (+ (screen-cursor-row s) 1))))

;; Answers #t when an edit to a line would produce a visible effect.
(define (interesting-change? old-line new-line column right-margin)
  (for/or [(i (in-range column right-margin))]
    (not (equal? (vector-ref old-line i) (vector-ref new-line i)))))

(define (repair-span! tty old new-line row first-col cell-count)
  (define trailing-empty-count
    (for/fold [(empty-count 0)] [(column (in-range first-col (+ first-col cell-count)))]
      (match-define (cons new-pen new-ch) (vector-ref new-line column))
      (if (non-empty? new-ch)
          (begin (set-pen tty new-pen)
                 (output tty (goto-if-needed old row column) new-ch)
                 (advance-cursor! tty old)
                 0)
          (+ empty-count 1))))
  (when (and (positive? trailing-empty-count) (= (+ first-col cell-count) (tty-columns tty)))
    (output tty (ansi:clear-to-eol))))

(define (repair-line! tty old new row)
  (define columns (screen-columns new))
  (define old-line (vector-ref (screen-contents old) row))
  (define new-line (vector-ref (screen-contents new) row))
  (define patches (diff-indices old-line new-line))
  (if (<= (length patches) 3)
      (apply-patch! patches
                    (lambda (first-col cols-to-remove)
                      (when (interesting-change? old-line new-line first-col columns)
                        (output tty (goto-if-needed old row first-col))
                        (delete-columns tty cols-to-remove)))
                    (lambda (first-col cols-to-insert cell-count)
                      (when (interesting-change? old-line new-line first-col columns)
                        (output tty (goto-if-needed old row first-col))
                        (when (and (positive? cols-to-insert)
                                   (interesting-change? old-line
                                                        new-line
                                                        (+ first-col cols-to-insert)
                                                        columns))
                          (insert-columns tty cols-to-insert))
                        (repair-span! tty old new-line row first-col cell-count))))
      (repair-span! tty old new-line row 0 columns)))

(define (tty-flush tty)
  (define old (tty-displayed-screen tty))
  (define new (tty-pending-screen tty))
  (apply-patch! (diff-indices (screen-contents old) (screen-contents new))
                (lambda (first-row lines-to-remove)
                  (output tty (goto-if-needed old first-row (screen-cursor-column old)))
                  (delete-lines tty lines-to-remove))
                (lambda (first-row lines-to-insert line-count)
                  (when (positive? lines-to-insert)
                    (output tty (goto-if-needed old first-row (screen-cursor-column old)))
                    (insert-lines tty lines-to-insert))
                  (for ((row (in-range first-row (+ first-row line-count))))
                    (repair-line! tty old new row))))
  (output tty (goto-if-needed old (screen-cursor-row new) (screen-cursor-column new)))
  (flush tty)
  (set-tty-displayed-screen! tty (struct-copy screen new [pen (screen-pen old)]))
  (set-tty-pending-screen! tty (copy-screen new))
  tty)

;;---------------------------------------------------------------------------
;; Input

(define (tty-next-key tty)
  (define k (ansi:lex-lcd-input (tty-input tty) #:utf-8? (tty-utf-8-input? tty)))
  (if (equal? k (ansi:key #\[ (set 'control))) ;; ESC
      (or (sync/timeout 0.5
                        (handle-evt (tty-next-key-evt tty)
                                    (lambda (k) (ansi:add-modifier 'meta k))))
          k)
      k))

(define (tty-next-key-evt tty)
  (handle-evt (tty-input tty)
              (lambda (_) (tty-next-key tty))))
