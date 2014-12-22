#lang racket/base

(provide (struct-out tty)
         tty-last-row
         tty-last-column
         stdin-tty
         tty-display
         tty-newline
         tty-clear
         tty-clear-to-eol
         tty-reset
         tty-goto
         tty-style
         tty-style-reset

         ;; From ansi
         color-black
         color-red
         color-green
         color-yellow
         color-blue
         color-magenta
         color-cyan
         color-white)

(require racket/match)
(require ansi)

(struct tty (input ;; InputPort
             output ;; OutputPort
             key-reader ;; InputPort -> Key
             [rows #:mutable] ;; Nat
             [columns #:mutable] ;; Nat
             [cursor-row #:mutable] ;; Nat
             [cursor-column #:mutable] ;; Nat
             [foreground-color #:mutable] ;; Nat
             [background-color #:mutable] ;; Nat
             [bold? #:mutable] ;; Boolean
             [italic? #:mutable] ;; Boolean
             ) #:transparent)

(define (tty-last-row t) (- (tty-rows t) 1))
(define (tty-last-column t) (- (tty-columns t) 1))

(define *stdin-tty* #f)
(define (stdin-tty)
  (when (not *stdin-tty*)
    (tty-raw!)
    (set! *stdin-tty*
          (tty (current-input-port)
               (current-output-port)
               lex-lcd-input
               24
               80
               0
               0
               color-white
               color-black
               #f
               #f))
    (tty-reset *stdin-tty*)
    (plumber-add-flush! (current-plumber)
                        (lambda (h)
                          (tty-style-reset *stdin-tty*)
                          (tty-goto *stdin-tty* (tty-last-row *stdin-tty*) 0))))
  *stdin-tty*)

(define (tty-display tty . items)
  (for ((i items)) (display i (tty-output tty)))
  (flush-output (tty-output tty)))

(define (tty-newline tty)
  (tty-display tty "\r\n"))

(define (tty-goto tty row0 column0)
  (define row (max 0 (min (tty-last-row tty) row0)))
  (define column (max 0 (min (tty-last-column tty) column0)))
  (tty-display tty (goto (+ row 1) (+ column 1)))
  (set-tty-cursor-row! tty row)
  (set-tty-cursor-column! tty column)
  tty)

(define (tty-clear tty)
  (tty-style tty) ;; applies style from tty
  (tty-display tty (clear-screen/home))
  (set-tty-cursor-row! tty 0)
  (set-tty-cursor-column! tty 0)
  tty)

(define (tty-clear-to-eol tty)
  (tty-display tty (clear-to-eol))
  tty)

(define (tty-style tty
                   #:foreground-color [fgcolor (tty-foreground-color tty)]
                   #:background-color [bgcolor (tty-background-color tty)]
                   #:bold? [bold? (tty-bold? tty)]
                   #:italic? [italic? (tty-italic? tty)])
  (tty-display tty
               (select-graphic-rendition)
               (apply select-graphic-rendition
                      `(,@(if bold? (list style-bold) (list))
                        ,@(if italic? (list style-italic/inverse) (list))
                        ,(style-text-color fgcolor)
                        ,(style-background-color bgcolor))))
  (set-tty-foreground-color! tty fgcolor)
  (set-tty-background-color! tty bgcolor)
  (set-tty-bold?! tty bold?)
  (set-tty-italic?! tty italic?)
  tty)

(define (tty-style-reset tty)
  (tty-style tty
             #:foreground-color color-white
             #:background-color color-black
             #:bold? #f
             #:italic? #f))

(define (collect-position-report tty)
  (let loop ()
    (sync/timeout 0.5
                  (handle-evt (tty-input tty)
                              (lambda (p)
                                (match ((tty-key-reader tty) p)
                                  [(? position-report? r) r]
                                  [_ (loop)]))))))

(define (tty-reset tty)
  (tty-display tty
               (clear-screen)
               (goto 999 999)
               (position-report-request))
  (define report (or (collect-position-report tty)
                     (position-report 24 80))) ;; TODO: have a more flexible fallback
  (tty-clear tty)
  (set-tty-rows! tty (position-report-row report))
  (set-tty-columns! tty (position-report-column report))
  tty)
