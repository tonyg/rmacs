#lang racket/base

(provide fundamental-mode)

(require racket/set)
(require racket/match)
(require (except-in racket/string string-prefix?))
(require "../api.rkt")
(require "../strings.rkt")

(define fundamental-mode
  (mode-add-constraints (make-mode "fundamental")
                        #:dispatch-keys-after '(#:minibuf)
                        #:interpret-commands-after '(#:minibuf)))

(define ((read-interactive-signature prompt) ed meta-sig meta-argname k)
  (define buf (editor-active-buffer ed))
  (define last-cmd (editor-last-command ed))
  (define sigs (for/hash [(sig (modeset-command-signatures (buffer-modeset buf)))
                          #:when (eq? (command-signature-category sig) 'interactive)]
                 (values (command-signature-selector sig) sig)))
  (completing-read ed prompt
                   (simple-completion (hash-keys sigs))
                   #:on-accept (lambda (content)
                                 (define selector (string->symbol content))
                                 (define sig (hash-ref sigs selector #f))
                                 (when (not sig)
                                   (abort "No such interactive command ~a" selector))
                                 (set-editor-last-command! ed last-cmd)
                                 (k sig)
                                 content)))

(define ((read-buffer prompt #:default-to-next? [default-to-next? #t]) ed sig argname k)
  (define buf (editor-active-buffer ed))
  (define default-target ((if default-to-next? buffer-next values) buf))
  (completing-read ed
                   (format "~a~a: "
                           prompt
                           (if default-target
                               (format " (default ~a)" (buffer-title default-target))
                               ""))
                   (simple-completion (buffergroup-buffer-titles (editor-buffers ed)))
                   #:on-accept (lambda (title0)
                                 (define title1 (string-trim title0))
                                 (define title (if (equal? title1 "") #f title1))
                                 (define target (if title (find-buffer ed title) default-target))
                                 (k target)
                                 (buffer-title target))))

(define ((read-filesystem-path) ed sig argname k)
  (local-require racket/path)
  (define buf (editor-active-buffer ed))
  (define src (and buf (buffer-source buf)))
  (completing-read ed
                   "Find file: "
                   (lambda (prefix0 string=?)
                     (define prefix (simplify-path prefix0))
                     (define-values (dirname filename)
                       (let-values (((d f dir?) (split-path prefix)))
                         (if dir?
                             (values (path->string prefix) #f)
                             (values (path->string d) (path->string f)))))
                     (for/list ((p (directory-list dirname))
                                #:when (or (not filename)
                                           (string-prefix? filename (path->string p) string=?)))
                       (define q (path->string (build-path dirname p)))
                       (if (directory-exists? q) (string-append q "/") q)))
                   #:initial
                   (if src
                       (string-append
                        (path->string (normalize-path
                                       (simplify-path (build-path (buffer-source-path src) 'up))))
                        "/")
                       (path->string (normalize-path ".")))
                   #:on-accept (lambda (str)
                                 (k str))))

(define-simple-command-signature (quoted-insert))
(define-simple-command-signature (newline))
(define-simple-command-signature (indent-for-tab-command))
(define-simple-command-signature (forward-char))
(define-simple-command-signature (backward-char))
(define-simple-command-signature (next-line))
(define-simple-command-signature (prev-line))
(define-simple-command-signature (move-end-of-line))
(define-simple-command-signature (move-beginning-of-line))
(define-simple-command-signature (delete-backward-char))
(define-simple-command-signature (delete-forward-char))
(define-simple-command-signature (beginning-of-buffer))
(define-simple-command-signature (end-of-buffer))
(define-simple-command-signature (exchange-point-and-mark))
(define-simple-command-signature (set-mark-command))
(define-simple-command-signature (split-window-below))
(define-simple-command-signature (delete-other-windows))
(define-simple-command-signature (delete-window))
(define-simple-command-signature (other-window))
(define-simple-command-signature (save-buffer))
(define-simple-command-signature
  (execute-extended-command [signature (read-interactive-signature "M-x ")]))
(define-simple-command-signature
  (switch-to-buffer [target-buffer (read-buffer "Switch to buffer")]))
(define-simple-command-signature
  (kill-buffer [target-buffer (read-buffer "Kill buffer" #:default-to-next? #f)]))
(define-simple-command-signature (kill-region))
(define-simple-command-signature (yank))
(define-simple-command-signature (yank-pop))
(define-simple-command-signature (append-next-kill))
(define-simple-command-signature (copy-region-as-kill))
(define-simple-command-signature (kill-ring-save))
(define-simple-command-signature (kill-line))
(define-simple-command-signature (undo))
(define-simple-command-signature (find-file [path (read-filesystem-path)]))
(define-simple-command-signature (scroll-up-command))
(define-simple-command-signature (scroll-down-command))

(define (default-search-pattern ed)
  (cond [(history-ref (minibuffer-history ed) 0) => list]
        [else '()]))

(define-simple-command-signature
  (search-forward [needle (string-arg "Search: " #:defaults default-search-pattern)]))
(define-simple-command-signature
  (search-backward [needle (string-arg "Search backward: " #:defaults default-search-pattern)]))
(define-simple-command-signature
  (search-forward-regexp [needle (string-arg "Regexp search: " #:defaults default-search-pattern)]))

(define (self-insert-command cmd)
  (match (command-keyseq cmd)
    [(list (key (? char? ch) modifiers)) #:when (set-empty? (set-remove modifiers 'shift))
     (buffer-insert! (command-buffer cmd)
                     (window-point (command-window cmd))
                     (string->rope (string ch)))]
    [_ #f]))

(define-command fundamental-mode cmd:unbound-key-sequence (#:command cmd)
  (self-insert-command cmd))

(define-command fundamental-mode cmd:quoted-insert (#:buffer buf #:window win #:keyseq keyseq)
  #:bind-key "C-q #:default"
  (match keyseq
    [(list _ (key (? char? ch) modifiers)) #:when (set-empty? (set-remove modifiers 'shift))
     (buffer-insert! buf (window-point win) (string->rope (string ch)))]
    [(list _ (key (? char? ch0) modifiers)) #:when (equal? modifiers (set 'control))
     (define ch (integer->char (- (char->integer (char-upcase ch0)) (char->integer #\A) -1)))
     (buffer-insert! buf (window-point win) (string->rope (string ch)))]
    [_ #f]))

(define-command fundamental-mode cmd:newline (#:buffer buf #:window win)
  #:bind-key "<return>"
  #:bind-key "C-j"
  (buffer-insert! buf (window-point win) (string->rope "\n")))

(define-command fundamental-mode cmd:indent-for-tab-command (#:buffer buf #:window win)
  #:bind-key "<tab>"
  (buffer-insert! buf (window-point win) (string->rope "\t")))

(define (plus-n-lines buf pos count)
  (for/fold [(pos pos)] [(i (in-range count))] (+ (buffer-end-of-line buf pos) 1)))

(define (minus-n-lines buf pos count)
  (for/fold [(pos pos)] [(i (in-range count))] (- (buffer-start-of-line buf pos) 1)))

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

(define-command fundamental-mode cmd:forward-char
  (#:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-f"
  #:bind-key "<right>"
  (buffer-move-mark! buf (window-point win) count))

(define-command fundamental-mode cmd:backward-char
  (#:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-b"
  #:bind-key "<left>"
  (buffer-move-mark! buf (window-point win) (- count)))

(define-buffer-local last-vertical-movement-preferred-column)

(define (vertical-movement-preferred-column editor win)
  (define buf (window-buffer win))
  (last-vertical-movement-preferred-column
   buf
   (or (and (editor-last-command? editor
                                  cmd:next-line
                                  cmd:prev-line)
            (last-vertical-movement-preferred-column buf))
       (buffer-column buf (window-point win)))))

(define-command fundamental-mode cmd:next-line (#:window win #:editor ed #:prefix-arg [count 1])
  #:bind-key "C-n"
  #:bind-key "<down>"
  (define col (vertical-movement-preferred-column ed win))
  (move-forward-n-lines win count)
  (move-to-column win col))

(define-command fundamental-mode cmd:prev-line (#:window win #:editor ed #:prefix-arg [count 1])
  #:bind-key "C-p"
  #:bind-key "<up>"
  (define col (vertical-movement-preferred-column ed win))
  (move-backward-n-lines win count)
  (move-to-column win col))

(define-command fundamental-mode cmd:move-end-of-line
  (#:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-e"
  #:bind-key "<end>"
  (when (positive? count) (move-forward-n-lines win (- count 1)))
  (buffer-move-mark-to-end-of-line! buf (window-point win)))

(define-command fundamental-mode cmd:move-beginning-of-line
  (#:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "C-a"
  #:bind-key "<home>"
  (when (positive? count) (move-forward-n-lines win (- count 1)))
  (buffer-move-mark-to-start-of-line! buf (window-point win)))

(define-command fundamental-mode cmd:delete-backward-char
  (#:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "<backspace>"
  #:bind-key "C-h" ;; differs from GNU emacs
  (define pos (buffer-mark-pos buf (window-point win)))
  (buffer-region-update! buf (- pos 1) pos (lambda (_deleted) (empty-rope))))

(define-command fundamental-mode cmd:delete-forward-char
  (#:buffer buf #:window win #:prefix-arg [count 1])
  #:bind-key "<delete>"
  #:bind-key "C-d"
  (define pos (buffer-mark-pos buf (window-point win)))
  (buffer-region-update! buf pos (+ pos 1) (lambda (_deleted) (empty-rope))))

(define (set-mark! win [pos (window-point win)] #:noisy? [noisy? #t])
  (buffer-mark! (window-buffer win) region-mark pos)
  (when (and noisy? (window-editor win)) (message (window-editor win) "Mark set"))
  pos)

(define-command fundamental-mode cmd:beginning-of-buffer
  (#:buffer buf #:window win #:prefix-arg [tenths 0])
  #:bind-key "M-<"
  #:bind-key "S-M-<"
  #:bind-key "S-M-," ;; OS X for some reason!
  #:bind-key "C-<home>"
  #:bind-key "<begin>"
  (if (eq? tenths '#:universal) (set! tenths 0) (set-mark! win))
  (window-move-to! win (* (buffer-size buf) (max 0 (min 10 tenths)) 1/10)))

(define-command fundamental-mode cmd:end-of-buffer
  (#:buffer buf #:window win #:prefix-arg [tenths 0])
  #:bind-key "M->"
  #:bind-key "S-M->"
  #:bind-key "S-M-." ;; OS X for some reason!
  #:bind-key "C-<end>"
  (if (eq? tenths '#:universal) (set! tenths 0) (set-mark! win))
  (window-move-to! win (* (buffer-size buf) (- 10 (max 0 (min 10 tenths))) 1/10)))

(define-command fundamental-mode cmd:exchange-point-and-mark (#:buffer buf #:window win)
  #:bind-key "C-x C-x"
  (define m (buffer-mark-pos* buf region-mark))
  (when m
    (set-mark! win #:noisy? #f)
    (window-move-to! win m)))

(define-command fundamental-mode cmd:set-mark-command (#:buffer buf #:window win #:prefix-arg arg)
  #:bind-key "C-@"
  #:bind-key "C-space"
  (if (eq? arg '#:universal)
      (let ((m (buffer-mark-pos* buf region-mark)))
        (and m (window-move-to! win m)))
      (set-mark! win)))

(define-command fundamental-mode cmd:split-window-below (#:buffer buf #:window win #:editor ed)
  #:bind-key "C-x 2"
  (open-window ed buf #:after-window win #:activate? #f))

(define-command fundamental-mode cmd:delete-other-windows (#:window win #:editor ed)
  #:bind-key "C-x 1"
  (close-other-windows ed win))

(define-command fundamental-mode cmd:delete-window (#:window win #:editor ed)
  #:bind-key "C-x 0"
  (close-window ed win))

(define-command fundamental-mode cmd:other-window (#:window win #:editor ed)
  #:bind-key "C-<tab>"
  #:bind-key "C-x o"
  (select-window ed (editor-next-window ed win)))

(define-command fundamental-mode cmd:save-buffer (#:buffer buf #:editor ed)
  #:bind-key "C-x C-s"
  (save-buffer! buf)
  (undo-list buf (map (match-lambda [(list was-dirty? pos old new) (list #t pos old new)])
                      (undo-list buf)))
  (message ed "Wrote ~a" (path->string (buffer-source-path (buffer-source buf)))))

(define-command fundamental-mode cmd:execute-extended-command
  (signature #:editor ed #:prefix-arg prefix)
  #:bind-key "M-x"
  (collect-args-and-invoke/history ed signature #f prefix))

(define-command fundamental-mode cmd:switch-to-buffer (target-buffer #:window win)
  #:bind-key "C-x b"
  (buffer-reorder! target-buffer)
  (set-window-buffer! win target-buffer))

(define-command fundamental-mode cmd:kill-buffer (target-buffer #:editor ed)
  #:bind-key "C-x k"
  (find-buffer ed "*scratch*") ;; side-effect: ensures a scratch buffer exists.
  (when (> (buffergroup-count (editor-buffers ed)) 1)
    ;; We don't do anything when there's just one buffer (i.e. the
    ;; scratch buffer, per side effect above) left, because otherwise
    ;; we'd be left with no buffers at all, which would leave our
    ;; windows with nothing to display, etc.
    (for [(win (windows-for-buffer ed target-buffer))]
      (when (eq? (window-buffer win) target-buffer)
        (set-window-buffer! win (buffer-next (window-buffer win)))))
    (register-buffer! #f target-buffer)))

(define-editor-local kill-ring*)
(define-command-local kill-command?)

(define (kill-ring editor)
  (when (not (kill-ring* editor))
    (kill-ring* editor (make-ring)))
  (kill-ring* editor))

(define (copy-region-as-kill! cmd ed region)
  (define full-region (if (kill-command? (editor-last-command ed))
                          (rope-append (ring-remove-item! (kill-ring ed)) region)
                          region))
  (ring-add-item! (kill-ring ed) (clear-all-marks full-region))
  (kill-command? cmd #t)
  region)

(define (kill-region! cmd ed buf pm1 pm2)
  (buffer-region-update! buf pm1 pm2 (lambda (region)
                                       (copy-region-as-kill! cmd ed region)
                                       (empty-rope))))

(define (yank! ed buf pm #:index [index 0])
  (define region (ring-ref (kill-ring ed) index))
  (buffer-insert! buf pm region))

(define (mark-pos-or-die buf)
  (or (buffer-mark-pos* buf region-mark)
      (abort "The mark is not set now, so there is no region")))

(define-command fundamental-mode cmd:kill-region
  (#:buffer buf #:window win #:editor ed #:command cmd)
  #:bind-key "C-w"
  #:bind-key "S-<delete>"
  (kill-region! cmd ed buf (window-point win) (mark-pos-or-die buf)))

(define-command fundamental-mode cmd:yank (#:buffer buf #:window win #:editor ed)
  #:bind-key "C-y"
  #:bind-key "S-<insert>"
  (set-mark! win)
  (yank! ed buf (window-point win)))

(define-command fundamental-mode cmd:yank-pop (#:buffer buf #:window win #:editor ed)
  #:bind-key "M-y"
  (if (editor-last-command? ed cmd:yank cmd:yank-pop)
      (buffer-region-update! buf (window-point win) (mark-pos-or-die buf)
                             (lambda (previously-yanked-region)
                               (ring-rotate! (kill-ring ed) 1)
                               (ring-ref (kill-ring ed))))
      (abort "Previous command was not a yank")))

(define-command fundamental-mode cmd:append-next-kill (#:command cmd #:editor ed)
  #:bind-key "C-M-w"
  (message ed "If the next command is a kill, it will append")
  (kill-command? cmd #t))

(define-command fundamental-mode cmd:copy-region-as-kill
  (#:buffer buf #:window win #:editor ed #:command cmd)
  (copy-region-as-kill! cmd ed (buffer-region buf (window-point win) (mark-pos-or-die buf))))

(define (temporarily-move-cursor-to ed win buf pos)
  (define point (buffer-mark-pos buf (window-point win)))
  (buffer-mark! buf (window-point win) pos)
  (editor-sit-for ed 1)
  (buffer-mark! buf (window-point win) point))

(define-command fundamental-mode cmd:kill-ring-save
  (#:buffer buf #:window win #:editor ed #:command cmd)
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

(define-command fundamental-mode cmd:kill-line
  (#:buffer buf #:window win #:editor ed #:command cmd #:prefix-arg count)
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

(define (search-in-buffer ed win buf mode needle)
  (when (positive? (string-length needle))
    (define pos+len
      (case mode
        [(forward) (buffer-search buf (window-point win) needle #:forward? #t)]
        [(backward) (buffer-search buf (window-point win) needle #:forward? #f)]
        [(forward-regexp) (buffer-search-regexp buf (window-point win) needle)]))
    (if (not pos+len)
        (message ed
                 (case mode
                   [(forward) "Failing search: ~a"]
                   [(backward) "Failing search backward: ~a"]
                   [(forward-regexp) "Failing regexp search: ~a"])
                 needle)
        (let ((newpos (+ (car pos+len)
                         (case mode
                           [(forward forward-regexp) (cdr pos+len)]
                           [(backward) 0]))))
          (set-mark! win #:noisy? #f)
          (message ed "Mark saved where search started")
          (buffer-mark! buf (window-point win) newpos)))
    needle))

(define-command fundamental-mode cmd:search-forward (needle #:buffer buf #:window win #:editor ed)
  #:bind-key "C-s"
  (search-in-buffer ed win buf 'forward needle))

(define-command fundamental-mode cmd:search-backward (needle #:buffer buf #:window win #:editor ed)
  #:bind-key "C-r"
  (search-in-buffer ed win buf 'backward needle))

(define-command fundamental-mode cmd:search-forward-regexp
  (needle #:buffer buf #:window win #:editor ed)
  #:bind-key "C-M-s"
  (search-in-buffer ed win buf 'forward-regexp needle))

(define-buffer-local undo-list '())
(define-command-local repeated-undo-list)

(define undo-insertion-coalesce-limit 20)

(define-command fundamental-mode cmd:buffer-changed
  (was-dirty? pos old-content new-content #:buffer buf)
  (undo-list buf
             (match (undo-list buf)
               [(cons (list prev-was-dirty? prev-pos (? rope-empty?) prev-insertion) rest)
                #:when (and (rope-empty? old-content)
                            (= prev-pos (- pos (rope-size prev-insertion)))
                            (< (+ (rope-size prev-insertion) (rope-size new-content))
                               undo-insertion-coalesce-limit))
                (cons (list prev-was-dirty?
                            prev-pos
                            old-content
                            (rope-append prev-insertion new-content))
                      rest)]
               [rest
                (cons (list was-dirty? pos old-content new-content) rest)])))

(define-command fundamental-mode cmd:undo (#:command cmd #:buffer buf #:window win #:editor ed)
  #:bind-key "C-_"
  #:bind-key "C-S-_"
  #:bind-key "C-/"
  #:bind-key "C-x u"
  (define actions (or (repeated-undo-list (editor-last-command ed)) (undo-list buf)))
  (match actions
    ['() (abort "No further undo information")]
    [(cons (list was-dirty? pos old-content new-content) rest)
     (repeated-undo-list cmd rest)
     (buffer-region-update! buf pos (+ pos (rope-size new-content))
                            (lambda (_new-content-again) old-content))
     (buffer-mark! buf (window-point win) (+ pos (rope-size old-content)))
     (when (not was-dirty?) (mark-buffer-clean! buf))]))

(define-command fundamental-mode cmd:find-file (path #:editor ed)
  #:bind-key "C-x C-f"
  (visit-file! ed path))

(define-command fundamental-mode cmd:scroll-up-command (#:buffer buf #:window win)
  #:bind-key "C-v"
  #:bind-key "<page-down>"
  (define new-pos (buffer-start-of-line buf (minus-n-lines buf (window-bottom win) 1)))
  (buffer-mark! buf (window-point win) new-pos)
  (buffer-mark! buf (window-top win) new-pos))

(define-command fundamental-mode cmd:scroll-down-command (#:buffer buf #:window win)
  #:bind-key "M-v"
  #:bind-key "<page-up>"
  (define scroll-count (- (window-available-line-count win) 2))
  (define new-top (buffer-start-of-line buf (minus-n-lines buf (window-top win) scroll-count)))
  (buffer-mark! buf (window-point win) (window-top win))
  (buffer-mark! buf (window-top win) new-top))
