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
             ) #:prefab)

(define (make-screen rows columns pen)
  (define contents (for/vector ((row rows)) (make-vector columns (cons pen #\space))))
  (screen rows columns 0 0 pen contents))

(define (copy-screen s)
  (match-define (screen rows columns cursor-row cursor-column pen contents) s)
  (define new-contents (for/vector ((row rows)) (vector-copy (vector-ref contents row))))
  (screen rows columns cursor-row cursor-column pen new-contents))

(define *pen-white-on-black* (pen ansi:color-white ansi:color-black #f #f))

(define *stdin-tty* #f)
(define (stdin-tty)
  (when (not *stdin-tty*)
    (ansi:tty-raw!)
    (set! *stdin-tty*
          (tty (current-input-port)
               (current-output-port)
               ansi:lex-lcd-input
               (make-screen 24 80 *pen-white-on-black*)
               (make-screen 24 80 *pen-white-on-black*)))
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
  (set-pen tty *pen-white-on-black* #:force #t)
  (clear tty)
  (flush tty)
  (set-tty-displayed-screen! tty (make-screen rows columns *pen-white-on-black*))
  (set-tty-pending-screen! tty (make-screen rows columns *pen-white-on-black*))
  tty)

(define (set-pen tty p #:force [force #f])
  (match-define (pen fgcolor bgcolor bold? italic?) p)
  (when (or force (not (equal? p (screen-pen (tty-displayed-screen tty)))))
    (output tty
            (apply ansi:select-graphic-rendition
                   `(,@(if bold? (list ansi:style-bold) (list))
                     ,@(if italic? (list ansi:style-italic/inverse) (list))
                     ,(ansi:style-text-color fgcolor)
                     ,(ansi:style-background-color bgcolor))))
    (set-screen-pen! (tty-displayed-screen tty) p))
  tty)

(define (clear tty)
  (output tty (ansi:clear-screen/home))
  (set-screen-cursor-row! (tty-displayed-screen tty) 0)
  (set-screen-cursor-column! (tty-displayed-screen tty) 0)
  tty)

(define (delete-lines tty n)
  (define s (tty-displayed-screen tty))
  (output tty (ansi:delete-lines n))
  (define blank-line (make-vector (screen-columns s) (cons 'unknown #\space)))
  (vector-copy! (screen-contents s)
                (screen-cursor-row s)
                (screen-contents s)
                (+ (screen-cursor-row s) n)
                (screen-rows s))
  (for ((i (in-range (- (screen-rows s) n) (screen-rows s))))
    (vector-set! (screen-contents s) i blank-line))
  tty)

(define (insert-lines tty n)
  (define s (tty-displayed-screen tty))
  (set-pen tty (car (vector-ref (vector-ref (screen-contents s)
                                            (max 0 (- (screen-cursor-row s) 1)))
                                (screen-cursor-column s))))
  (output tty (ansi:insert-lines n))
  (define blank-line (make-vector (screen-columns s) (cons (screen-pen s) #\space)))
  (vector-copy! (screen-contents s)
                (+ (screen-cursor-row s) n)
                (screen-contents s)
                (screen-cursor-row s)
                (- (screen-rows s) n))
  (for ((i (in-range (screen-cursor-row s) (+ (screen-cursor-row s) n))))
    (vector-set! (screen-contents s) i blank-line))
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

(define (putc tty ch)
  (define s (tty-pending-screen tty))
  (match ch
    [#\return
     (tty-goto tty (screen-cursor-row s) 0)]
    [#\newline
     (tty-goto tty (+ (screen-cursor-row s) 1) (screen-cursor-column s))]
    [#\tab
     (for ((i (- 8 (modulo (screen-cursor-column s) 8)))) (putc tty #\space))]
    [(? char-iso-control?)
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
  (putc tty #\return)
  (putc tty #\newline))

(define (tty-clear tty)
  (set-tty-pending-screen! tty (make-screen (tty-rows tty) (tty-columns tty) (tty-pen tty)))
  tty)

(define (tty-clear-to-eol tty)
  (define start-column (tty-cursor-column tty))
  (for ((i (- (tty-columns tty) (tty-cursor-column tty)))) (putc tty #\space))
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
;;                      #:when (not (equal? (cdr cell) #\space)))
;;            (cdr cell)))))

(define (goto-if-needed s row column)
  (if (and (= (screen-cursor-row s) row)
           (= (screen-cursor-column s) column))
      ""
      (begin0 (ansi:goto (+ row 1) (+ column 1))
              (set-screen-cursor-row! s row)
              (set-screen-cursor-column! s column))))

(define (advance-cursor! s)
  (set-screen-cursor-column! s (+ (screen-cursor-column s) 1))
  (when (= (screen-cursor-column s) (screen-columns s))
    (set-screen-cursor-column! s 0)
    (set-screen-cursor-row! s (+ (screen-cursor-row s) 1))))

(define (tty-flush tty)
  ;; (set-pen tty *pen-white-on-black* #:force #t)
  ;; (clear tty)
  (define old (tty-displayed-screen tty))
  (define new (tty-pending-screen tty))
  (define patches (diff-indices (screen-contents old) (screen-contents new)))

  ;; Proceed in two stages:
  ;;  - delete unwanted lines
  ;;  - insert and blank lines

  (for/fold [(skew 0)]
            [(patch patches)]
    (match-define (list patch-old-line patch-old-count patch-new-line patch-new-count) patch)
    (define delta-lines (- patch-new-count patch-old-count))
    (define first-row (+ patch-old-line skew))
    (if (negative? delta-lines)
        (begin (output tty (goto-if-needed old first-row (screen-cursor-column old)))
               (delete-lines tty (- delta-lines))
               (+ skew delta-lines))
        skew))

  (for/fold [(skew 0)]
            [(patch patches)]
    (match-define (list patch-old-line patch-old-count patch-new-line patch-new-count) patch)
    (define delta-lines (- patch-new-count patch-old-count))
    (define first-row (+ patch-old-line skew))
    (when (positive? delta-lines)
      (output tty (goto-if-needed old first-row (screen-cursor-column old)))
      (insert-lines tty delta-lines))
    (for ((row (in-range first-row (+ patch-old-line skew patch-new-count))))
      (define old-line (vector-ref (screen-contents old) row))
      (define new-line (vector-ref (screen-contents new) row))
      ;; TODO: consider diffing old-line and new-line and applying
      ;; patches rather than just blitting out the whole line
      ;; whereever it is different.
      (for ((column (screen-columns new)))
        (match-define (cons old-pen old-ch) (vector-ref old-line column))
        (match-define (cons new-pen new-ch) (vector-ref new-line column))
        (when (not (and (equal? old-pen new-pen) (equal? old-ch new-ch)))
          (set-pen tty new-pen)
          (output tty (goto-if-needed old row column) new-ch)
          (advance-cursor! old))))
    (+ skew delta-lines))

  (output tty (goto-if-needed old (screen-cursor-row new) (screen-cursor-column new)))
  (flush tty)
  (set-tty-displayed-screen! tty new)
  (set-tty-pending-screen! tty (copy-screen new))
  tty)

;;---------------------------------------------------------------------------
;; Input

(define (tty-next-key tty)
  (define k (ansi:lex-lcd-input (tty-input tty)))
  (if (equal? k (ansi:key #\[ (set 'control))) ;; ESC
      (or (sync/timeout 0.5
                        (handle-evt (tty-next-key-evt tty)
                                    (lambda (k) (ansi:add-modifier 'meta k))))
          k)
      k))

(define (tty-next-key-evt tty)
  (handle-evt (tty-input tty)
              (lambda (_) (tty-next-key tty))))
