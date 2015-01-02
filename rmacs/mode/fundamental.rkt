#lang racket/base

(provide fundamental-mode)

(require racket/set)
(require racket/match)
(require racket/string)
(require "../api.rkt")

(define fundamental-mode (make-mode "fundamental"))

(define-command fundamental-mode (self-insert-command #:buffer buf #:window win #:keyseq keyseq)
  (match keyseq
    [(list (key (? char? ch) modifiers)) #:when (set-empty? (set-remove modifiers 'shift))
     (buffer-insert! buf (window-point win) (string->rope (string ch)))]
    [_ #f]))

(define-command fundamental-mode (unbound-key-sequence #:command cmd #:keyseq keyseq)
  (invoke (copy-command cmd #:selector 'self-insert-command)))

(define-command fundamental-mode (quoted-insert #:buffer buf #:window win #:keyseq keyseq)
  #:bind-key "C-q #:default"
  (match keyseq
    [(list _ (key (? char? ch) modifiers)) #:when (set-empty? (set-remove modifiers 'shift))
     (buffer-insert! buf (window-point win) (string->rope (string ch)))]
    [(list _ (key (? char? ch0) modifiers)) #:when (equal? modifiers (set 'control))
     (define ch (integer->char (- (char->integer (char-upcase ch0)) (char->integer #\A) -1)))
     (buffer-insert! buf (window-point win) (string->rope (string ch)))]
    [_ #f]))

(define-command fundamental-mode (newline #:buffer buf #:window win)
  #:bind-key "C-m"
  #:bind-key "C-j"
  (buffer-insert! buf (window-point win) (string->rope "\n")))

(define-command fundamental-mode (indent-for-tab-command #:buffer buf #:window win)
  #:bind-key "C-i"
  (buffer-insert! buf (window-point win) (string->rope "\t")))

(define (plus-n-lines buf pos count)
  (for/fold [(pos pos)] [(i count)] (+ (buffer-end-of-line buf pos) 1)))

(define (minus-n-lines buf pos count)
  (for/fold [(pos pos)] [(i count)] (- (buffer-start-of-line buf pos) 1)))

(define (move-forward-n-lines win count)
  (define buf (window-buffer win))
  (buffer-mark! buf (window-point win) (plus-n-lines buf (window-point win) count)))

(define (move-backward-n-lines win count)
  (define buf (window-buffer win))
  (buffer-mark! buf (window-point win) (minus-n-lines buf (window-point win) count)))

(define (move-to-column win col)
  (define buf (window-buffer win))
  (define sol (buffer-start-of-line buf (window-point win)))
  (buffer-mark! buf (window-point win) (buffer-closest-pos-for-column buf sol 0 col)))

(define-command fundamental-mode (forward-char #:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-f"
  #:bind-key "<right>"
  (buffer-move-mark! buf (window-point win) count))

(define-command fundamental-mode (backward-char #:buffer buf #:window win #:prefix-arg [count 1])
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

(define-command fundamental-mode (next-line #:window win #:editor ed #:prefix-arg [count 1])
  #:bind-key "C-n"
  #:bind-key "<down>"
  (define col (vertical-movement-preferred-column ed win))
  (move-forward-n-lines win count)
  (move-to-column win col))

(define-command fundamental-mode (prev-line #:window win #:editor ed #:prefix-arg [count 1])
  #:bind-key "C-p"
  #:bind-key "<up>"
  (define col (vertical-movement-preferred-column ed win))
  (move-backward-n-lines win count)
  (move-to-column win col))

(define-command fundamental-mode (move-end-of-line #:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-e"
  #:bind-key "<end>"
  (when (positive? count) (move-forward-n-lines win (- count 1)))
  (buffer-move-mark-to-end-of-line! buf (window-point win)))

(define-command fundamental-mode
  (move-beginning-of-line #:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-a"
  #:bind-key "<home>"
  (when (positive? count) (move-forward-n-lines win (- count 1)))
  (buffer-move-mark-to-start-of-line! buf (window-point win)))

(define-command fundamental-mode
  (delete-backward-char #:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "<backspace>"
  #:bind-key "C-h" ;; differs from GNU emacs
  (define pos (buffer-mark-pos buf (window-point win)))
  (buffer-region-update! buf (- pos 1) pos (lambda (_deleted) (empty-rope))))

(define-command fundamental-mode
  (delete-forward-char #:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "<delete>"
  #:bind-key "C-d"
  (define pos (buffer-mark-pos buf (window-point win)))
  (buffer-region-update! buf pos (+ pos 1) (lambda (_deleted) (empty-rope))))

(define (set-mark! win [pos (window-point win)] #:noisy? [noisy? #t])
  (buffer-mark! (window-buffer win) region-mark pos)
  (when (and noisy? (window-editor win)) (message (window-editor win) "Mark set"))
  pos)

(define-command fundamental-mode
  (beginning-of-buffer #:buffer buf #:window win #:prefix-arg [tenths 0])
  #:bind-key "M-<"
  #:bind-key "C-<home>"
  #:bind-key "<begin>"
  (if (eq? tenths '#:universal) (set! tenths 0) (set-mark! win))
  (window-move-to! win (* (buffer-size buf) (max 0 (min 10 tenths)) 1/10)))

(define-command fundamental-mode (end-of-buffer #:buffer buf #:window win #:prefix-arg [tenths 0])
  #:bind-key "M->"
  #:bind-key "C-<end>"
  (if (eq? tenths '#:universal) (set! tenths 0) (set-mark! win))
  (window-move-to! win (* (buffer-size buf) (- 10 (max 0 (min 10 tenths))) 1/10)))

(define-command fundamental-mode (exchange-point-and-mark #:buffer buf #:window win)
  #:bind-key "C-x C-x"
  (define m (buffer-mark-pos* buf region-mark))
  (when m
    (set-mark! win #:noisy? #f)
    (window-move-to! win m)))

(define-command fundamental-mode (set-mark-command #:buffer buf #:window win #:prefix-arg arg)
  #:bind-key "C-@"
  #:bind-key "C-space"
  (if (eq? arg '#:universal)
      (let ((m (buffer-mark-pos* buf region-mark)))
        (and m (window-move-to! win m)))
      (set-mark! win)))

(define-command fundamental-mode (split-window-below #:buffer buf #:window win #:editor ed)
  #:bind-key "C-x 2"
  (open-window ed buf #:after-window win #:activate? #f))

(define-command fundamental-mode (delete-other-windows #:window win #:editor ed)
  #:bind-key "C-x 1"
  (close-other-windows ed win))

(define-command fundamental-mode (delete-window #:window win #:editor ed)
  #:bind-key "C-x 0"
  (close-window ed win))

(define-command fundamental-mode (other-window #:window win #:editor ed)
  #:bind-key "C-tab"
  #:bind-key "C-x o"
  (select-window ed (editor-next-window ed win)))

(define-command fundamental-mode (save-buffer #:buffer buf)
  #:bind-key "C-x C-s"
  (save-buffer! buf))

(define-command fundamental-mode (execute-extended-command #:buffer buf #:command cmd #:editor ed)
  #:bind-key "M-x"
  (define last-cmd (editor-last-command ed))
  (completing-read ed "M-x "
                   (simple-completion (modeset-command-selectors (buffer-modeset buf)))
                   #:on-accept (lambda (content)
                                 (define selector (string->symbol content))
                                 (set-editor-last-command! ed last-cmd)
                                 (invoke/history (copy-command cmd
                                                               #:selector (string->symbol content)
                                                               #:keyseq #f)))))

(define-command fundamental-mode (switch-to-buffer #:buffer buf #:window win #:editor ed)
  #:bind-key "C-x b"
  (define default-target (buffer-next buf))
  (completing-read ed
                   (format "Switch to buffer~a: "
                           (if default-target
                               (format " (default ~a)" (buffer-title default-target))
                               ""))
                   (simple-completion (buffergroup-buffer-titles (editor-buffers ed)))
                   #:on-accept (lambda (title0)
                                 (define title1 (string-trim title0))
                                 (define title (if (equal? title1 "") #f title1))
                                 (define target (if title (find-buffer ed title) default-target))
                                 (set-window-buffer! win target))))

(define-editor-local kill-ring*)
(define-command-local kill-command?)

(define (kill-ring editor)
  (when (not (kill-ring* editor))
    (kill-ring* editor (make-ring)))
  (kill-ring* editor))

(define (copy-region-as-kill! cmd ed region)
  (define full-region (if (kill-command? (editor-last-command ed))
                          (rope-append (ring-unpush! (kill-ring ed)) region)
                          region))
  (ring-push! (kill-ring ed) (clear-all-marks full-region))
  (kill-command? cmd #t)
  region)

(define (kill-region! cmd ed buf pm1 pm2)
  (buffer-region-update! buf pm1 pm2 (lambda (region)
                                       (copy-region-as-kill! cmd ed region)
                                       (empty-rope))))

(define (yank! ed buf pm #:index [index 0])
  (define region (ring-peek (kill-ring ed) index))
  (buffer-insert! buf pm region))

(define (mark-pos-or-die buf)
  (or (buffer-mark-pos* buf region-mark)
      (abort "The mark is not set now, so there is no region")))

(define-command fundamental-mode (kill-region #:buffer buf #:window win #:editor ed #:command cmd)
  #:bind-key "C-w"
  #:bind-key "S-<delete>"
  (kill-region! cmd ed buf (window-point win) (mark-pos-or-die buf)))

(define-command fundamental-mode (yank #:buffer buf #:window win #:editor ed)
  #:bind-key "C-y"
  #:bind-key "S-<insert>"
  (set-mark! win)
  (yank! ed buf (window-point win)))

(define-command fundamental-mode (yank-pop #:buffer buf #:window win #:editor ed)
  #:bind-key "M-y"
  (if (editor-last-command? ed 'yank 'yank-pop)
      (buffer-region-update! buf (window-point win) (mark-pos-or-die buf)
                             (lambda (previously-yanked-region)
                               (ring-pop! (kill-ring ed) 1)))
      (abort "Previous command was not a yank")))

(define-command fundamental-mode (append-next-kill #:command cmd #:editor ed)
  #:bind-key "C-M-w"
  (message ed "If the next command is a kill, it will append")
  (kill-command? cmd #t))

(define-command fundamental-mode
  (copy-region-as-kill #:buffer buf #:window win #:editor ed #:command cmd)
  (copy-region-as-kill! cmd ed (buffer-region buf (window-point win) (mark-pos-or-die buf))))

(define (temporarily-move-cursor-to ed win buf pos)
  (define point (buffer-mark-pos buf (window-point win)))
  (buffer-mark! buf (window-point win) pos)
  (editor-sit-for ed 1)
  (buffer-mark! buf (window-point win) point))

(define-command fundamental-mode
  (kill-ring-save #:buffer buf #:window win #:editor ed #:command cmd)
  #:bind-key "M-w"
  #:bind-key "C-<insert>"
  (define mark (mark-pos-or-die buf))
  (define point (buffer-mark-pos buf (window-point win)))
  (define region (copy-region-as-kill! cmd ed (buffer-region buf point mark)))
  (if (position-visible? win mark)
      (temporarily-move-cursor-to ed win buf mark)
      (let-values (((lo hi) (if (< point mark)
                                (values (- mark 40) mark)
                                (values mark (+ mark 40)))))
        (define snippet (rope->string (buffer-region buf lo hi)))
        (message ed "Saved text ~a \"~a\"" (if (< point mark) "until" "from") snippet))))

(define-command fundamental-mode
  (kill-line #:buffer buf #:window win #:editor ed #:command cmd #:prefix-arg count)
  #:bind-key "C-k"
  (define point (buffer-mark-pos buf (window-point win)))
  (define-values (start end)
    (cond
     [(eq? count '#:default)
      (define eol (buffer-end-of-line buf (window-point win)))
      (if (= point eol)
          (values point (+ point 1))
          (values point eol))]
     [(positive? count)
      (values (window-point win)
              (buffer-end-of-line buf (plus-n-lines buf (window-point win) (- count 1))))]
     [else
      (values (buffer-start-of-line buf (minus-n-lines buf (window-point win) (- count)))
              (window-point win))]))
  (kill-region! cmd ed buf start end))
