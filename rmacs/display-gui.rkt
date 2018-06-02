#lang racket/base
;; Implicitly provides a factory via display.rkt's `register-tty-backend!`.

(require racket/set)
(require racket/match)
(require racket/class)
(require racket/gui/base)
(require racket/generic)
(require "display.rkt")

(struct gui (frame ;; frame%
             canvas ;; canvas%
             keys-ch ;; Channel
             [key-pushback #:mutable] ;; (Option Key)
             [screen #:mutable] ;; Screen
             )
  #:methods gen:tty
  [(define (tty-pending-screen t) (gui-screen t))
   (define (set-tty-pending-screen! t s) (set-gui-screen! t s))
   (define (tty-reset t) (gui-reset t))
   (define (tty-flush t) (send (gui-canvas t) refresh-now))
   (define (tty-set-title! t title) (send (gui-frame t) set-label title))
   (define (tty-next-key t) (gui-next-key t))
   (define (tty-next-key-evt t) (gui-next-key-evt t))
   (define (tty-input-available-evt t) (gui-input-available-evt t))])

(define (gui-factory)
  (and (or (eq? (system-type) 'macosx)
	   (eq? (system-type) 'windows)
           (getenv "DISPLAY"))
       (not (getenv "RMACS_NO_GUI"))
       (gui-factory*)))

(define (gui-factory*)
  (parameterize ((current-eventspace (make-eventspace)))
    (define keys-ch (make-channel))
    (define frame (new frame%
                       [label "rmacs"]
                       [width 640]
                       [height 480]))
    (define g 'uninitialized-gui)
    (define (gui-initialized?) (not (eq? g 'uninitialized-gui)))
    (define canvas
      (new (class canvas%
             (super-new)
             ;; (define/override (on-event e)
             ;;   (fprintf (current-error-port) "\r\nevent ~v\r\n" e))
             (define/override (on-size _w _h)
               (when (gui-initialized?) (gui-reset g))
               (channel-put keys-ch (key 'window-resize (set))))
             (define/override (on-char e)
               (define key-code (send e get-key-code))
               (when (not (set-member? (set 'release
                                            'shift
                                            'rshift
                                            'control
                                            'rcontrol)
                                       key-code))
                 (define control? (send e get-control-down))
                 (channel-put
                  keys-ch
                  (key (if (and control? (char-alphabetic? key-code))
                           (char-upcase key-code)
                           (match key-code
                             ['prior 'page-up]
                             ['next 'page-down]
                             [#\backspace 'backspace]
                             [#\rubout 'delete]
                             [#\return 'return]
                             [#\tab 'tab]
                             [other other]))
                       (list->set
                        (filter values
                                (list (and (send e get-shift-down) 'shift)
                                      (and (send e get-alt-down) 'meta)
                                      (and (send e get-meta-down) 'meta)
                                      (and control? 'control))))))))
             )
           [parent frame]
           [paint-callback
            (lambda (canvas dc)
              (when (gui-initialized?) (gui-render g canvas dc)))]))
    (set! g (gui frame
                 canvas
                 keys-ch
                 #f
                 (make-screen 24 80 tty-default-pen)))
    (send canvas focus)
    (send frame show #t)
    (gui-reset g)
    g))

(define (mkf bold? italic?)
  (define size (if (eq? (system-type) 'windows) 11 14))
  (define name (if (eq? (system-type) 'windows) "Consolas" "Inconsolata"))
  (make-object font% size name 'default
               (if italic? 'italic 'normal)
               (if bold? 'bold 'normal)))

(define *ad-hoc-scale* (if (eq? (system-type) 'windows) 1.3 1))

(define fonts
  (vector (mkf #f #f)
          (mkf #f #t)
          (mkf #t #f)
          (mkf #t #t)))

(define (gui-render g canvas dc)
  (define s (gui-screen g))

  (define (lookup-color n)
    (match n
      [(== color-black) "black"]
      [(== color-red) "red"]
      [(== color-green) "green"]
      [(== color-yellow) "yellow"]
      [(== color-blue) "blue"]
      [(== color-magenta) "magenta"]
      [(== color-cyan) "cyan"]
      [(== color-white) "white"]))

  (send dc set-font (vector-ref fonts 0))
  (send dc set-text-mode 'solid)
  (send dc set-background "black")
  (send dc clear)

  (define default-pen (pen color-white color-black #f #f))
  (define cursor-pen (pen color-white color-green #t #f))

  (for/fold [(y-offset 0)]
            [(row (in-range (screen-rows s)))]
    (define current-row-height (* *ad-hoc-scale* (send dc get-char-height)))
    (define x-offset 0)
    (let loop ((col 0)
               (acc-rev '())
               (pen #f))
      (define (finish-chunk!)
        (when (pair? acc-rev)
          (let ((pen (if (eq? pen 'default) default-pen pen)))
            (define line (list->string (reverse acc-rev)))
            (send dc set-font (vector-ref fonts
                                          (+ (if (pen-bold? pen) 2 0)
                                             (if (pen-italic? pen) 1 0))))
            (send dc set-text-foreground (lookup-color (pen-foreground-color pen)))
            (send dc set-text-background (lookup-color (pen-background-color pen)))
            (define-values (w h _descender _verticalspace)
              (send dc get-text-extent line))
            (send dc draw-text line x-offset y-offset)
            (set! x-offset (+ x-offset w))
            (set! current-row-height (max current-row-height h)))))
      (if (= col (screen-columns s))
          (finish-chunk!)
          (match (vector-ref (vector-ref (screen-contents s) row) col)
            [(cons new-pen ch)
             (when (and (= col (screen-cursor-column s))
                        (= row (screen-cursor-row s)))
               (set! new-pen cursor-pen)
               (when (eq? ch 'empty)
                 (set! ch #\space)))
             (cond
               [(eq? ch 'empty)
                (loop (+ col 1) acc-rev pen)]
               [(or (equal? new-pen pen) (not pen))
                (loop (+ col 1) (cons ch acc-rev) new-pen)]
               [else
                (finish-chunk!)
                (loop (+ col 1) (list ch) new-pen)])])))
    (+ y-offset current-row-height)))

(define (gui-reset g)
  (define canvas (gui-canvas g))
  (define-values (width height) (send canvas get-client-size))
  (define dc (send canvas get-dc))
  (send dc set-font (vector-ref fonts 0))
  (define-values (rows columns)
    (values (- (inexact->exact (floor (/ height (* *ad-hoc-scale* (send dc get-char-height))))) 0)
            (- (inexact->exact (floor (/ width (send dc get-char-width)))) 0)))
  (set-gui-screen! g (make-screen rows columns tty-default-pen))
  g)

(define (gui-next-key g)
  (define pb (gui-key-pushback g))
  (if pb
      (begin (set-gui-key-pushback! g #f)
             pb)
      (channel-get (gui-keys-ch g))))

(define (gui-next-key-evt g)
  (define pb (gui-key-pushback g))
  (if pb
      (handle-evt always-evt
                  (lambda (_)
                    (set-gui-key-pushback! g #f)
                    pb))
      (gui-keys-ch g)))

(define (gui-input-available-evt g)
  (define pb (gui-key-pushback g))
  (if pb
      always-evt
      (handle-evt (gui-keys-ch g)
                  (lambda (k)
                    (set-gui-key-pushback! g k)
                    #t))))

(register-tty-backend! 'gui gui-factory #:priority 1)
