#lang racket/base

(provide fundamental-mode)

(require racket/set)
(require racket/match)
(require "../api.rkt")

(define fundamental-mode (make-mode "fundamental"))

(define-command fundamental-mode (self-insert-command buf #:window win #:keyseq keyseq)
  (match keyseq
    [(list (key (? char? ch) modifiers)) #:when (set-empty? (set-remove modifiers 'shift))
     (buffer-insert! buf (window-point win) (string->rope (string ch)))]
    [_ #f]))

(define-command fundamental-mode (unbound-key-sequence buf #:command cmd #:keyseq keyseq)
  (invoke (copy-command cmd #:selector 'self-insert-command)))

(define-command fundamental-mode (quoted-insert buf #:window win #:keyseq keyseq)
  #:bind-key "C-q #:default"
  (match keyseq
    [(list _ (key (? char? ch) modifiers)) #:when (set-empty? (set-remove modifiers 'shift))
     (buffer-insert! buf (window-point win) (string->rope (string ch)))]
    [(list _ (key (? char? ch0) modifiers)) #:when (equal? modifiers (set 'control))
     (define ch (integer->char (- (char->integer (char-upcase ch0)) (char->integer #\A) -1)))
     (buffer-insert! buf (window-point win) (string->rope (string ch)))]
    [_ #f]))

(define-command fundamental-mode (newline buf #:window win)
  #:bind-key "C-m"
  #:bind-key "C-j"
  (buffer-insert! buf (window-point win) (string->rope "\n")))

(define (move-forward-n-lines win count)
  (define buf (window-buffer win))
  (for ((i count))
    (buffer-move-mark-to-end-of-line! buf (window-point win))
    (buffer-move-mark! buf (window-point win) 1)))

(define (move-backward-n-lines win count)
  (define buf (window-buffer win))
  (for ((i count))
    (buffer-move-mark-to-start-of-line! buf (window-point win))
    (buffer-move-mark! buf (window-point win) -1)))

(define (move-to-column win col)
  (define buf (window-buffer win))
  (define sol (buffer-start-of-line buf (window-point win)))
  (buffer-mark! buf (window-point win) (buffer-closest-pos-for-column buf sol 0 col)))

(define-command fundamental-mode (forward-char buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-f"
  #:bind-key "<right>"
  (buffer-move-mark! buf (window-point win) count))

(define-command fundamental-mode (backward-char buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-b"
  #:bind-key "<left>"
  (buffer-move-mark! buf (window-point win) (- count)))

(define-buffer-local last-vertical-movement-preferred-column)

(define (vertical-movement-preferred-column editor win)
  (define buf (window-buffer win))
  (last-vertical-movement-preferred-column
   buf
   (or (and (editor-last-command? editor
                                  'next-line
                                  'prev-line)
            (last-vertical-movement-preferred-column buf))
       (buffer-column buf (window-point win)))))

(define-command fundamental-mode (next-line buf #:window win #:editor ed #:prefix-arg [count 1])
  #:bind-key "C-n"
  #:bind-key "<down>"
  (define col (vertical-movement-preferred-column ed win))
  (move-forward-n-lines win count)
  (move-to-column win col))

(define-command fundamental-mode (prev-line buf #:window win #:editor ed #:prefix-arg [count 1])
  #:bind-key "C-p"
  #:bind-key "<up>"
  (define col (vertical-movement-preferred-column ed win))
  (move-backward-n-lines win count)
  (move-to-column win col))

(define-command fundamental-mode (move-end-of-line buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-e"
  #:bind-key "<end>"
  (when (positive? count) (move-forward-n-lines win (- count 1)))
  (buffer-move-mark-to-end-of-line! buf (window-point win)))

(define-command fundamental-mode (move-beginning-of-line buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-a"
  #:bind-key "<home>"
  (when (positive? count) (move-forward-n-lines win (- count 1)))
  (buffer-move-mark-to-start-of-line! buf (window-point win)))

(define-command fundamental-mode (delete-backward-char buf #:window win #:prefix-arg [count 1])
  #:bind-key "<backspace>"
  #:bind-key "C-h" ;; differs from GNU emacs
  (define pos (buffer-mark-pos buf (window-point win)))
  (buffer-region-update! buf (- pos 1) pos (lambda (_deleted) (empty-rope))))

(define-command fundamental-mode (delete-forward-char buf #:window win #:prefix-arg [count 1])
  #:bind-key "<delete>"
  #:bind-key "C-d"
  (define pos (buffer-mark-pos buf (window-point win)))
  (buffer-region-update! buf pos (+ pos 1) (lambda (_deleted) (empty-rope))))

(define (set-window-mark! win [pos (window-point win)])
  (window-mark! win pos)
  (message (window-editor win) "Mark set")
  pos)

(define-command fundamental-mode (beginning-of-buffer buf #:window win #:prefix-arg [tenths 0])
  #:bind-key "M-<"
  #:bind-key "C-<home>"
  #:bind-key "<begin>"
  (if (eq? tenths '#:prefix) (set! tenths 0) (set-window-mark! win))
  (window-move-to! win (* (buffer-size buf) (max 0 (min 10 tenths)) 1/10)))

(define-command fundamental-mode (end-of-buffer buf #:window win #:prefix-arg [tenths 0])
  #:bind-key "M->"
  #:bind-key "C-<end>"
  (if (eq? tenths '#:prefix) (set! tenths 0) (set-window-mark! win))
  (window-move-to! win (* (buffer-size buf) (- 10 (max 0 (min 10 tenths))) 1/10)))

(define-command fundamental-mode (exchange-point-and-mark buf #:window win)
  #:bind-key "C-x C-x"
  (define m (buffer-mark-pos* buf (window-mark win)))
  (when m
    (window-mark! win)
    (window-move-to! win m)))

(define-command fundamental-mode (set-mark-command buf #:window win #:prefix-arg arg)
  #:bind-key "C-@"
  #:bind-key "C-space"
  (if (eq? arg '#:prefix)
      (let ((m (buffer-mark-pos* buf (window-mark win))))
        (and m (window-move-to! win m)))
      (set-window-mark! win)))

(define-command fundamental-mode (split-window-below buf #:window win #:editor ed)
  #:bind-key "C-x 2"
  (open-window ed buf #:after-window win #:activate? #f))

(define-command fundamental-mode (delete-other-windows buf #:window win #:editor ed)
  #:bind-key "C-x 1"
  (close-other-windows ed win))

(define-command fundamental-mode (delete-window buf #:window win #:editor ed)
  #:bind-key "C-x 0"
  (close-window ed win))

(define-command fundamental-mode (other-window buf #:window win #:editor ed)
  #:bind-key "C-tab"
  #:bind-key "C-x o"
  (select-window ed (editor-next-window ed win)))

(define-command fundamental-mode (save-buffer buf)
  #:bind-key "C-x C-s"
  (save-buffer! buf))

(define-command fundamental-mode (execute-extended-command buf #:command cmd #:editor ed)
  #:bind-key "M-x"
  (completing-read ed "M-x "
                   (simple-completion (modeset-command-selectors (buffer-modeset buf)))
                   #:on-accept (lambda (content)
                                 (define selector (string->symbol content))
                                 (invoke (copy-command cmd
                                                       #:selector (string->symbol content)
                                                       #:keyseq #f)))))
