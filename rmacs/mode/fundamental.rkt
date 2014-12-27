#lang racket/base

(provide fundamental-mode)

(require racket/set)
(require racket/match)
(require "../api.rkt")

(define fundamental-mode (make-mode "fundamental"))

(define-command fundamental-mode (self-insert-command buf #:keyseq keyseq)
  (match keyseq
    [(list (key (? char? ch) modifiers)) #:when (set-empty? (set-remove modifiers 'shift))
     (buffer-insert! buf (string->rope (string ch)))]
    [_ #f]))

(define-command fundamental-mode (unbound-key-sequence buf #:keyseq keyseq)
  (invoke (command 'self-insert-command buf #:keyseq keyseq)))

(define-key fundamental-mode (list "C-q" '#:default) self-insert-command)

(define-command fundamental-mode (newline buf)
  #:bind-key "C-m"
  #:bind-key "C-j"
  (buffer-insert! buf (string->rope "\n")))

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

(define-command fundamental-mode (forward-char buf #:prefix-arg [count 1])
  #:bind-key "C-f"
  #:bind-key "<right>"
  (buffer-move-by! buf count))

(define-command fundamental-mode (backward-char buf #:prefix-arg [count 1])
  #:bind-key "C-b"
  #:bind-key "<left>"
  (buffer-move-by! buf (- count)))

(define-command fundamental-mode (next-line buf #:prefix-arg [count 1])
  #:bind-key "C-n"
  #:bind-key "<down>"
  (define col (buffer-column buf))
  (move-forward-n-lines buf count)
  (move-to-column buf col))

(define-command fundamental-mode (prev-line buf #:prefix-arg [count 1])
  #:bind-key "C-p"
  #:bind-key "<up>"
  (define col (buffer-column buf))
  (move-backward-n-lines buf count)
  (move-to-column buf col))

(define-command fundamental-mode (move-end-of-line buf #:prefix-arg [count 1])
  #:bind-key "C-e"
  #:bind-key "<end>"
  (when (positive? count) (move-forward-n-lines buf (- count 1)))
  (buffer-move-to-end-of-line! buf))

(define-command fundamental-mode (move-beginning-of-line buf #:prefix-arg [count 1])
  #:bind-key "C-a"
  #:bind-key "<home>"
  (when (positive? count) (move-forward-n-lines buf (- count 1)))
  (buffer-move-to-start-of-line! buf))

(define-command fundamental-mode (delete-backward-char buf #:prefix-arg [count 1])
  #:bind-key "<backspace>"
  #:bind-key "C-h" ;; differs from GNU emacs
  (buffer-region-update! buf
                         (lambda (_deleted) (empty-rope))
                         #:mark (- (buffer-pos buf) count)))

(define-command fundamental-mode (delete-forward-char buf #:prefix-arg [count 1])
  #:bind-key "<delete>"
  #:bind-key "C-d"
  (buffer-region-update! buf
                         (lambda (_deleted) (empty-rope))
                         #:mark (+ (buffer-pos buf) count)))

(define-command fundamental-mode (beginning-of-buffer buf #:prefix-arg [tenths 0])
  #:bind-key "M-<"
  #:bind-key "C-<home>"
  #:bind-key "<begin>"
  (if (eq? tenths '#:prefix) (set! tenths 0) (buffer-mark! buf))
  (buffer-move-to! buf (* (buffer-size buf) (max 0 (min 10 tenths)) 1/10)))

(define-command fundamental-mode (end-of-buffer buf #:prefix-arg [tenths 0])
  #:bind-key "M->"
  #:bind-key "C-<end>"
  (if (eq? tenths '#:prefix) (set! tenths 0) (buffer-mark! buf))
  (buffer-move-to! buf (* (buffer-size buf) (- 10 (max 0 (min 10 tenths))) 1/10)))

(define-command fundamental-mode (exchange-point-and-mark buf)
  #:bind-key "C-x C-x"
  (define m (buffer-mark-pos buf))
  (when m
    (define p (buffer-pos buf))
    (buffer-mark! buf p)
    (buffer-move-to! buf m)))

(define-command fundamental-mode (set-mark-command buf #:prefix-arg arg)
  #:bind-key "C-@"
  #:bind-key "C-space"
  (if (eq? arg '#:prefix)
      (let ((m (buffer-mark-pos buf)))
        (and m (buffer-move-to! buf m)))
      (buffer-mark! buf)))
