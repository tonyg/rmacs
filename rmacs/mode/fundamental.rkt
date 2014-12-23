#lang racket/base

(provide fundamental-mode)

(require ansi/lcd-terminal)
(require "../mode.rkt")
(require "../editor.rkt")
(require "../buffer.rkt")
(require "../keys.rkt")
(require "../rope.rkt")

(define fundamental-mode (make-mode "fundamental"))

(void (mode-keymap-bind! fundamental-mode
                         "ESC"
                         (lambda (e ks)
                           (key-macro-expansion (cons (add-modifier 'meta (car ks)) (cdr ks))))))

(define-command fundamental-mode (self-insert-command e #:keyseq keyseq)
  (define ch (key-value (car (reverse keyseq))))
  (when (char? ch)
    (buffer-insert! (current-editor-buffer e) (string->rope (string ch)))))

(define-command fundamental-mode (unbound-key-sequence e #:keyseq keyseq)
  (editor-invoke-command 'self-insert-command e #:keyseq keyseq))

(define-key fundamental-mode (list "C-q" '#:default) self-insert-command)

(define-command fundamental-mode (newline e)
  #:bind-key "C-m"
  #:bind-key "C-j"
  (buffer-insert! (current-editor-buffer e) (string->rope "\n")))

(define (move-forward-n-lines buf count)
  (for ((i count))
    (buffer-move-to-end-of-line! buf)
    (buffer-move-by! buf 1)))

(define (move-backward-n-lines buf count)
  (for ((i count))
    (buffer-move-to-start-of-line! buf)
    (buffer-move-by! buf -1)))

(define (move-to-column buf col)
  (define eol-pos (buffer-end-of-line buf))
  (buffer-move-to-start-of-line! buf)
  (buffer-move-by! buf (min col (- eol-pos (buffer-pos buf)))))

(define-command fundamental-mode (forward-char e #:prefix-arg [count 1])
  #:bind-key "C-f"
  #:bind-key "<right>"
  (buffer-move-by! (current-editor-buffer e) count))

(define-command fundamental-mode (backward-char e #:prefix-arg [count 1])
  #:bind-key "C-b"
  #:bind-key "<left>"
  (buffer-move-by! (current-editor-buffer e) (- count)))

(define-command fundamental-mode (next-line e #:prefix-arg [count 1])
  #:bind-key "C-n"
  #:bind-key "<down>"
  (define buf (current-editor-buffer e))
  (define col (buffer-column buf))
  (move-forward-n-lines buf count)
  (move-to-column buf col))

(define-command fundamental-mode (prev-line e #:prefix-arg [count 1])
  #:bind-key "C-p"
  #:bind-key "<up>"
  (define buf (current-editor-buffer e))
  (define col (buffer-column buf))
  (move-backward-n-lines buf count)
  (move-to-column buf col))

(define-command fundamental-mode (move-end-of-line e #:prefix-arg [count 1])
  #:bind-key "C-e"
  #:bind-key "<end>"
  (define buf (current-editor-buffer e))
  (when (positive? count) (move-forward-n-lines buf (- count 1)))
  (buffer-move-to-end-of-line! buf))

(define-command fundamental-mode (move-beginning-of-line e #:prefix-arg [count 1])
  #:bind-key "C-a"
  #:bind-key "<home>"
  (define buf (current-editor-buffer e))
  (when (positive? count) (move-forward-n-lines buf (- count 1)))
  (buffer-move-to-start-of-line! buf))

(define-command fundamental-mode (delete-backward-char e #:prefix-arg [count 1])
  #:bind-key "<backspace>"
  #:bind-key "C-h" ;; differs from GNU emacs
  (define buf (current-editor-buffer e))
  (buffer-region-update! buf
                         (lambda (_deleted) (empty-rope))
                         #:mark (- (buffer-pos buf) count)))

(define-command fundamental-mode (delete-forward-char e #:prefix-arg [count 1])
  #:bind-key "<delete>"
  #:bind-key "C-d"
  (define buf (current-editor-buffer e))
  (buffer-region-update! buf
                         (lambda (_deleted) (empty-rope))
                         #:mark (+ (buffer-pos buf) count)))

(define-command fundamental-mode (beginning-of-buffer e #:prefix-arg [tenths 0])
  #:bind-key "M-<"
  #:bind-key "C-<home>"
  #:bind-key "<begin>"
  (define buf (current-editor-buffer e))
  (if (eq? tenths '#:prefix) (set! tenths 0) (buffer-mark! buf))
  (buffer-move-to! buf (* (buffer-size buf) (max 0 (min 10 tenths)) 1/10)))

(define-command fundamental-mode (end-of-buffer e #:prefix-arg [tenths 0])
  #:bind-key "M->"
  #:bind-key "C-<end>"
  (define buf (current-editor-buffer e))
  (if (eq? tenths '#:prefix) (set! tenths 0) (buffer-mark! buf))
  (buffer-move-to! buf (* (buffer-size buf) (- 10 (max 0 (min 10 tenths))) 1/10)))

(define-command fundamental-mode (exchange-point-and-mark e)
  #:bind-key "C-x C-x"
  (define buf (current-editor-buffer e))
  (define m (buffer-mark-pos buf))
  (when m
    (define p (buffer-pos buf))
    (buffer-mark! buf p)
    (buffer-move-to! buf m)))

(define-command fundamental-mode (set-mark-command e #:prefix-arg arg)
  #:bind-key "C-@"
  #:bind-key "C-space"
  (define buf (current-editor-buffer e))
  (if (eq? arg '#:prefix)
      (let ((m (buffer-mark-pos buf)))
        (and m (buffer-move-to! buf m)))
      (buffer-mark! buf)))
