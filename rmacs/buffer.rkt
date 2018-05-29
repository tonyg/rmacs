#lang racket/base

(provide (struct-out buffer-mark-type)
         region-mark
         point-mark
         make-buffergroup
         initialize-buffergroup!
         buffergroup-buffer-titles
         buffergroup-count
         buffergroup-empty?
         buffer?
         make-buffer
         register-buffer!
         lookup-buffer
         unused-buffer-title
         load-buffer
         revert-buffer!
         save-buffer!
         buffer-rename!
         buffer-reorder!
         buffer-next
         buffer-prev
         buffer-title
         buffer-dirty?
         buffer-source
         buffer-rope
         buffer-group
         buffer-locals
         mark-buffer-clean!
         buffer-editor
         buffer-modeset
         buffer-string-column-count
         buffer-column
         buffer-closest-pos-for-column
         buffer-apply-modeset!
         buffer-add-mode!
         buffer-remove-mode!
         buffer-toggle-mode!
         buffer-size
         buffer-start-of-line
         buffer-end-of-line
         buffer-mark-types
         buffer-mark*
         buffer-mark
         buffer-mark-pos*
         buffer-mark-pos
         buffer-pos*
         buffer-pos
         buffer-mark!
         buffer-clear-mark!
         buffer-clear-all-marks/type!
         buffer-move-mark!
         buffer-move-mark-to-start-of-line!
         buffer-move-mark-to-end-of-line!
         buffer-region-split
         buffer-region
         cmd:buffer-changed
         buffer-region-update!
         buffer-insert!
         buffer-replace-contents!
         buffer-search
         buffer-findf
         buffer-search-regexp
         define-buffer-local
         define-command-local

         (except-out (struct-out command) command)
         (struct-out exn:abort)
         abort
         copy-command
         (rename-out [make-command command])
         invoke

         define-command)

(require racket/match)
(require (for-syntax syntax/parse))
(require (for-syntax racket/base))
(require (only-in racket/string string-join))
(require (only-in racket/path normalize-path))
(require (only-in racket/file file->string))

(require "rope.rkt")
(require "mark.rkt")
(require "search.rkt")
(require "circular-list.rkt")
(require "mode.rkt")
(require "keys.rkt")
(require "file.rkt")
(require "local.rkt")

(struct buffer-mark-type (kind ;; Symbol
                          window-id ;; (Option Symbol)
                          preserve? ;; Boolean
                          ) #:prefab)

(struct buffergroup ([members #:mutable] ;; (CircularList Buffer)
                     [editor #:mutable] ;; (Option Editor), for bidirectional editor/group linkage
                     ) #:prefab)

(struct buffer ([rope #:mutable]
                [title #:mutable]
                [group #:mutable] ;; (Option BufferGroup)
                [modeset #:mutable] ;; ModeSet
                [dirty? #:mutable] ;; Boolean
                [source #:mutable] ;; (Option BufferSource)
                [locals #:mutable] ;; LocalsTable
                ) #:prefab)

(struct exn:abort exn (detail duration) #:transparent)

(struct command (command-signature ;; CommandSignature
                 args ;; (Option (Listof Any))
                 buffer ;; Buffer
                 window ;; (Option Window)
                 editor ;; Editor
                 keyseq ;; (Option Keyseq)
                 prefix-arg ;; Any
                 [locals #:mutable] ;; LocalsTable
                 ) #:prefab)

(define region-mark (mark-type (buffer-mark-type 'mark #f #f) 'left))
(define point-mark  (mark-type (buffer-mark-type 'buffer-point #f #t) 'right))

(define (make-buffergroup)
  (buffergroup circular-empty #f))

(define (initialize-buffergroup! g editor)
  (when (buffergroup-editor g)
    (error 'initialize-buffergroup! "Duplicate initialization of buffergroup"))
  (set-buffergroup-editor! g editor)
  g)

(define (buffergroup-buffer-titles g)
  (map buffer-title (circular-list->list (buffergroup-members g))))

(define (buffergroup-count g) (circular-length (buffergroup-members g)))
(define (buffergroup-empty? g) (circular-null? (buffergroup-members g)))

(define (initial-contents-rope initial-contents)
  (cond
   [(string? initial-contents) (piece->rope initial-contents)]
   [(rope? initial-contents) initial-contents]
   [(procedure? initial-contents) (initial-contents-rope (initial-contents))]
   [else (error 'initial-contents-rope "Invalid initial-contents: ~v" initial-contents)]))

(define (make-buffer group ;; (Option BufferGroup)
                     title ;; String
                     #:initial-contents [initial-contents ""])
  (register-buffer! group (buffer (initial-contents-rope initial-contents)
                                  title
                                  #f
                                  kernel-modeset
                                  #f
                                  #f
                                  (make-locals))))

(define (register-buffer! group buf)
  (define old-group (buffer-group buf))
  (when old-group
    (set-buffergroup-members! old-group
                              (circular-list-remove buf (buffergroup-members old-group) eq?))
    (set-buffer-group! buf #f))
  (cond
   [(not group) buf]
   [(title->buffer* group (buffer-title buf)) #f]
   [else
    (set-buffer-group! buf group)
    (set-buffergroup-members! group (circular-cons buf (buffergroup-members group)))
    buf]))

(define (title->buffer* group title)
  (and group
       (circular-list-memf (lambda (b) (equal? (buffer-title b) title))
                           (buffergroup-members group))))

(define (buffer->buffer* group b)
  (and group
       (circular-list-memf (lambda (b1) (eq? b b1)) (buffergroup-members group))))

(define (lookup-buffer group title)
  (cond [(title->buffer* group title) => circular-car] [else #f]))

(define (title-exists-in-group? group title)
  (and (title->buffer* group title) #t))

(define (unused-buffer-title group context-pieces)
  (define primary-piece (if (null? context-pieces) "*anonymous*" (car context-pieces)))
  (define uniquifiers (if (null? context-pieces) '() (cdr context-pieces)))
  (let search ((used '()) (remaining uniquifiers))
    (define candidate
      (if (null? used)
          primary-piece
          (format "~a<~a>" primary-piece (string-join used "/"))))
    (if (title-exists-in-group? group candidate)
        (if (pair? remaining)
            (search (cons (car remaining) used) (cdr remaining))
            (let search ((counter 2))
              (define candidate (format "~a<~a>" primary-piece counter))
              (if (title-exists-in-group? group candidate)
                  (search (+ counter 1))
                  candidate)))
        candidate)))

(define (load-buffer group src)
  (define pieces (buffer-source-title-pieces src))
  (define title (if (not group) (car pieces) (unused-buffer-title group pieces)))
  (define b (make-buffer group title))
  (set-buffer-source! b src)
  (revert-buffer! b)
  b)

(define (revert-buffer! buf)
  (buffer-replace-contents! buf (piece->rope (buffer-source-read (buffer-source buf))))
  (mark-buffer-clean! buf))

(define (save-buffer! buf)
  (buffer-source-write (buffer-source buf) (rope->searchable-string (buffer-rope buf)))
  (mark-buffer-clean! buf))

(define (buffer-rename! b new-title)
  (if (title-exists-in-group? (buffer-group b) new-title)
      #f
      (begin (set-buffer-title! b new-title)
             b)))

(define (buffer-reorder! b)
  ;; Reorders b to the top of the group as a side-effect
  (register-buffer! (buffer-group b) b))

(define (buffer-next b)
  (cond [(buffer->buffer* (buffer-group b) b) => (compose circular-car circular-list-rotate-forward)]
        [else #f]))

(define (buffer-prev b)
  (cond [(buffer->buffer* (buffer-group b) b) => (compose circular-car circular-list-rotate-backward)]
        [else #f]))

(define (buffer-size buf) (rope-size (buffer-rope buf)))

(define (mark-buffer-clean! buf)
  (set-buffer-dirty?! buf #f))

(define (buffer-editor b)
  (define g (buffer-group b))
  (and g (buffergroup-editor g)))

(define (buffer-string-column-count buf start-column str)
  (for/fold [(count 0)] [(ch str)]
    (match ch
      [#\tab (+ count (- 8 (modulo (+ start-column count) 8)))]
      [#\newline (- start-column)]
      [_ (+ count 1)])))

(define (buffer-column buf pos-or-mtype)
  (define pos (->pos buf pos-or-mtype 'buffer-column))
  (define str (rope->searchable-string (buffer-region buf (buffer-start-of-line buf pos) pos)))
  (buffer-string-column-count buf 0 str))

(define (buffer-closest-pos-for-column buf sol-pos column-offset column)
  (define g (rope->searchable-generator (subrope (buffer-rope buf) sol-pos)))
  (let loop ((column-count column-offset) (pos sol-pos))
    (cond
     [(< column-count column)
      (match (g)
        [#\tab (loop (+ column-count (- 8 (modulo column-count 8))) (+ pos 1))]
        [#\newline pos]
        [(? char?) (loop (+ column-count 1) (+ pos 1))]
        [_ pos])]
     [(= column-count column) pos]
     [(> column-count column) (- pos 1)])))

(define (buffer-apply-modeset! buf modeset)
  (set-buffer-modeset! buf modeset))

(define (buffer-add-mode! buf mode)
  (set-buffer-modeset! buf (modeset-add-mode (buffer-modeset buf) mode)))
(define (buffer-remove-mode! buf mode)
  (set-buffer-modeset! buf (modeset-remove-mode (buffer-modeset buf) mode)))
(define (buffer-toggle-mode! buf mode)
  (set-buffer-modeset! buf (modeset-toggle-mode (buffer-modeset buf) mode)))

(define (clamp pos buf)
  (max 0 (min (buffer-size buf) pos)))

(define (buffer-seek! buf pos)
  (buffer-lift rope-seek buf (clamp pos buf)))

(define (newline? ch) (equal? ch #\newline))
(define (buffer-start-of-line buf pm) (buffer-findf buf pm newline? #:forward? #f))
(define (buffer-end-of-line buf pm)   (buffer-findf buf pm newline? #:forward? #t))

(define (->pos* buf pos-or-mtype what)
  (if (number? pos-or-mtype)
      (clamp pos-or-mtype buf)
      (let ((p (buffer-mark-pos* buf pos-or-mtype)))
        (and p (clamp p buf)))))

(define (error-mark-type-not-found what mtype buf)
  (error what "Mark type ~v not found; available mark types ~v" mtype (buffer-mark-types buf)))

(define (->pos buf pos-or-mtype what)
  (or (->pos* buf pos-or-mtype what)
      (error-mark-type-not-found what pos-or-mtype buf)))

(define (buffer-mark-types buf)
  (rope-marks (buffer-rope buf)))

(define (buffer-mark* buf mtype
                      #:forward? [forward? #t]
                      #:position [start-pos (if forward? 0 (buffer-size buf))])
  (find-in-index (buffer-rope buf) mtype
                 #:forward? forward?
                 #:position (->pos buf start-pos 'buffer-mark*)))

(define (buffer-mark buf mtype [what 'buffer-mark]
                     #:forward? [forward? #t]
                     #:position [start-pos (if forward? 0 (buffer-size buf))])
  (or (buffer-mark* buf mtype
                    #:forward? forward?
                    #:position (->pos buf start-pos 'buffer-mark))
      (error-mark-type-not-found what mtype buf)))

(define (buffer-mark-pos* buf mtype
                          #:forward? [forward? #t]
                          #:position [start-pos (if forward? 0 (buffer-size buf))])
  (find-pos-in-index (buffer-rope buf) mtype
                     #:forward? forward?
                     #:position (->pos buf start-pos 'buffer-mark-pos*)))

(define (buffer-mark-pos buf mtype [what 'buffer-mark-pos]
                         #:forward? [forward? #t]
                         #:position [start-pos (if forward? 0 (buffer-size buf))])
  (or (buffer-mark-pos* buf mtype
                        #:forward? forward?
                        #:position (->pos buf start-pos 'buffer-mark-pos))
      (error-mark-type-not-found what mtype buf)))

(define (buffer-pos* buf pos-or-mtype)
  (->pos* buf pos-or-mtype 'buffer-pos*))

(define (buffer-pos buf pos-or-mtype)
  (->pos buf pos-or-mtype 'buffer-pos))

(define (buffer-mark! buf mtype pos-or-mtype #:value [value #t] #:replace? [replace? #t])
  (buffer-lift (if replace? replace-mark set-mark)
               buf
               mtype
               (->pos buf pos-or-mtype 'buffer-mark!)
               value))

(define (buffer-clear-mark! buf mtype)
  (define pos (buffer-mark-pos* buf mtype))
  (if pos
      (buffer-lift clear-mark buf mtype pos)
      buf))

(define (buffer-clear-all-marks/type! buf mtype pm1 pm2)
  (define-values (l _lo m _hi r) (buffer-region-split buf pm1 pm2))
  (set-buffer-rope! buf (rope-append l (rope-append (clear-all-marks/type m mtype) r)))
  buf)

(define (buffer-move-mark! buf mtype delta)
  (match-define (cons pos val) (buffer-mark buf mtype 'buffer-move-mark!))
  (buffer-mark! buf mtype (+ pos delta) #:value val))

(define (buffer-move-mark-to-start-of-line! buf mtype)
  (define pos (buffer-mark-pos buf mtype 'buffer-move-mark-to-start-of-line!))
  (buffer-mark! buf mtype (buffer-start-of-line buf pos)))

(define (buffer-move-mark-to-end-of-line! buf mtype)
  (define pos (buffer-mark-pos buf mtype 'buffer-move-mark-to-end-of-line!))
  (buffer-mark! buf mtype (buffer-end-of-line buf pos)))

(define (buffer-region-split buf pm1 pm2)
  (define p1 (->pos buf pm1 'buffer-region-split))
  (define p2 (->pos buf pm2 'buffer-region-split))
  (define lo (min p1 p2))
  (define hi (max p1 p2))
  (define-values (l mr) (rope-split (buffer-rope buf) lo))
  (define-values (m r) (rope-split mr (- hi lo)))
  (values l lo m hi r))

(define (buffer-region buf pm1 pm2)
  (define-values (_l _lo m _hi _r) (buffer-region-split buf pm1 pm2))
  m)

(define (transfer-marks ro rn)
  (define mtypes-to-transfer
    (for/list ((mtype (rope-marks ro))
               #:when (and (buffer-mark-type? (mark-type-info mtype))
                           (buffer-mark-type-preserve? (mark-type-info mtype))))
      mtype))
  (for/fold [(rn rn)] [(mtype mtypes-to-transfer)]
    (define pos (case (mark-type-stickiness mtype)
                  [(left) 0]
                  [(right) (rope-size rn)]))
    (set-mark rn mtype pos #t)))

(define-simple-command-signature
  (buffer-changed [was-dirty? (const-arg #f)]
                  [pos (const-arg 0)]
                  [old-content (const-arg (rope-empty))]
                  [new-content (const-arg (rope-empty))])
  #:category event)

(define (send-edit-notification! buf was-dirty? pos old-content new-content)
  (invoke (make-command cmd:buffer-changed buf #:args (list was-dirty? pos old-content new-content))
          #:error-if-unhandled? #f))

(define (buffer-region-update! buf pm1 pm2 updater #:notify? [notify? #t])
  (define-values (l lo old-m hi r) (buffer-region-split buf pm1 pm2))
  (define new-m (transfer-marks old-m (updater old-m)))
  (set-buffer-rope! buf (rope-append (rope-append l new-m) r))
  (define old-dirty (buffer-dirty? buf))
  (when (buffer-source buf) (set-buffer-dirty?! buf #t))
  (when notify? (send-edit-notification! buf old-dirty lo old-m new-m))
  buf)

(define (buffer-insert! buf pos-or-mtype content-rope #:notify? [notify? #t])
  (define pos (->pos buf pos-or-mtype 'buffer-insert!))
  (define-values (l r) (rope-split (buffer-rope buf) pos))
  (set-buffer-rope! buf (rope-append (rope-append l content-rope) r))
  (define old-dirty (buffer-dirty? buf))
  (when (buffer-source buf) (set-buffer-dirty?! buf #t))
  (when notify? (send-edit-notification! buf old-dirty pos (rope-empty) content-rope))
  buf)

(define (buffer-replace-contents! buf content-rope)
  (buffer-region-update! buf 0 (buffer-size buf) (lambda (_dontcare) content-rope)))

(define (buffer-search* buf start-pos-or-mtype forward? find-delta)
  (define start-pos (->pos buf start-pos-or-mtype 'buffer-search*))
  (define-values (l r) (rope-split (buffer-rope buf) start-pos))
  (define delta+len (find-delta (if forward? r l)))
  (and delta+len ;; find-delta may return #f ...
       (car delta+len) ;; ... or (cons #f _) to signal failure
       (let ((new-pos (clamp (+ start-pos (if forward?
                                              (car delta+len)
                                              (- (car delta+len) (rope-size l))))
                             buf)))
         (buffer-seek! buf new-pos)
         (cons new-pos (cdr delta+len)))))

(define (buffer-search buf start-pos-or-mtype needle #:forward? [forward? #t])
  (buffer-search* buf start-pos-or-mtype forward?
                  (lambda (piece)
                    (cons (search-rope needle piece #:forward? forward?)
                          (string-length needle)))))

(define (buffer-findf buf start-pos-or-mtype f #:forward? [forward? #t])
  (car (buffer-search* buf start-pos-or-mtype forward?
                       (lambda (piece)
                         (cons (findf-in-rope f piece #:forward? forward?) 0)))))

(define (buffer-search-regexp buf start-pos-or-mtype pattern)
  (buffer-search* buf start-pos-or-mtype #t
                  (lambda (piece)
                    (define p (rope->searchable-port piece #:name (buffer-title buf)))
                    (match (regexp-match-positions pattern p)
                      [#f #f]
                      [(cons (cons lo hi) _)
                       ;; regexp-match-positions gives BYTE offsets,
                       ;; but we need CODEPOINT offsets. Reread,
                       ;; count, and discard the portion of the buffer
                       ;; leading up to the match.
                       (define p (rope->searchable-port piece #:name (buffer-title buf)))
                       (define prefix-len
                         (string-length (bytes->string/utf-8 (read-bytes lo p))))
                       (define match-len
                         (string-length (bytes->string/utf-8 (read-bytes (- hi lo) p))))
                       (cons prefix-len match-len)]))))

(define-local-definer define-buffer-local buffer-locals set-buffer-locals!)

(define-local-definer define-command-local
  (lambda (c) (if c (command-locals c) (make-locals)))
  (lambda (c ls) (when c (set-command-locals! c ls))))

(define (buffer-lift f buf . args)
  (define new-rope (apply f (buffer-rope buf) args))
  (set-buffer-rope! buf new-rope)
  buf)

;;---------------------------------------------------------------------------

(define (abort #:detail [detail #f]
               #:duration [duration #f]
               fmt . args)
  (raise (exn:abort (apply format fmt args)
                    (current-continuation-marks)
                    detail
                    duration)))

(define (make-command signature buffer-or-command
                      #:args args
                      #:window [window #f]
                      #:editor [editor #f]
                      #:keyseq [keyseq #f]
                      #:prefix-arg [prefix-arg '#:default])
  (define buffer (cond
                  [(buffer? buffer-or-command) buffer-or-command]
                  [(command? buffer-or-command) (command-buffer buffer-or-command)]))
  (command signature
           args
           buffer
           window
           (or editor (buffer-editor buffer))
           keyseq
           prefix-arg
           (make-locals)))

(define (copy-command cmd
                      #:args [args (command-args cmd)]
                      #:signature [signature (command-command-signature cmd)]
                      #:buffer [buffer (command-buffer cmd)]
                      #:window [window (command-window cmd)]
                      #:editor [editor (command-editor cmd)]
                      #:keyseq [keyseq (command-keyseq cmd)]
                      #:prefix-arg [prefix-arg (command-prefix-arg cmd)]
                      #:locals [locals (command-locals cmd)])
  (struct-copy command cmd
               [command-signature signature]
               [args args]
               [buffer buffer]
               [window window]
               [editor editor]
               [keyseq keyseq]
               [prefix-arg prefix-arg]
               [locals locals]))

(define (invoke cmd #:error-if-unhandled? [error-if-unhandled? #t])
  (match-define (command signature _ buf _ _ keyseq _ _) cmd)
  (define handler (modeset-lookup-command (buffer-modeset buf) signature))
  (cond
   [handler (handler cmd)]
   [error-if-unhandled?
    (abort "Unhandled command ~a (key sequence: ~a)"
           (command-signature->list signature)
           (if keyseq (keyseq->keyspec keyseq) "N/A"))]
   [else (void)]))

(define-syntax define-command
  (lambda (stx)
    (syntax-parse stx
      [(_ mode-exp sig-exp
          ((~or (~optional (~seq #:next-method next-method)
                           #:defaults ([next-method #'nm])
                           #:name "#:next-method")
                (~optional (~seq #:command cmd)
                           #:defaults ([cmd #'cmd])
                           #:name "#:command")
                (~optional (~seq #:selector self-selector)
                           #:defaults ([self-selector #'self])
                           #:name "#:selector")
                (~optional (~seq #:args args)
                           #:defaults ([args #'args])
                           #:name "#:args")
                (~optional (~seq #:buffer buffer)
                           #:defaults ([buffer #'buf])
                           #:name "#:buffer")
                (~optional (~seq #:window window)
                           #:defaults ([window #'win])
                           #:name "#:window")
                (~optional (~seq #:editor editor)
                           #:defaults ([editor #'ed])
                           #:name "#:editor")
                (~optional (~seq #:keyseq keyseq)
                           #:defaults ([keyseq #'keyseq])
                           #:name "#:keyseq")
                (~optional (~seq #:prefix-arg
                                 (~or (~seq [prefix-arg prefix-default prefix-universal])
                                      (~seq [prefix-arg prefix-default])
                                      prefix-arg))
                           #:defaults ([prefix-arg #'pa]
                                       [prefix-default #''#:default]
                                       [prefix-universal #''#:universal])
                           #:name "#:prefix-arg")
                argname)
           ...)
          (~seq #:bind-key bind-keyspec-exps) ...
          body ...)
       #`(let* ((mode mode-exp)
                (sig sig-exp)
                (argnames (list 'argname ...)))
           (when (not (equal? argnames
                              (map command-argument-spec-name (command-signature-args sig))))
             (error 'define-command
                    "Command handler for ~a in mode ~a expects args ~a"
                    (command-signature->list sig)
                    (mode-id mode)
                    argnames))
           (mode-define-command! mode
                                 sig
                                 (lambda (cmd next-method)
                                   (match-define (command self-selector
                                                          (and args (list argname ...))
                                                          buffer
                                                          window
                                                          editor
                                                          keyseq
                                                          prefix-arg
                                                          _) cmd)
                                   (let ((prefix-arg (match prefix-arg
                                                       ['#:default prefix-default]
                                                       ['#:universal prefix-universal]
                                                       [_ prefix-arg])))
                                     body ...)))
           #,@(for/list ((bind-keyspec-exp (syntax->list #'(bind-keyspec-exps ...))))
                #`(mode-keymap-bind! mode #,bind-keyspec-exp sig))
           (void))])))
