#lang racket/base

(provide make-buffergroup
         main-mark-type
         buffer?
         make-buffer
         register-buffer!
         lookup-buffer
         file->buffer
         buffer-rename!
         buffer-reorder!
         buffer-next
         buffer-prev
         buffer-pos
         buffer-title
         buffer-group
         buffer-size
         buffer-move-to!
         buffer-move-by!
         buffer-mark!
         buffer-clear-mark!
         buffer-mark-pos
         buffer-region-split
         buffer-region
         buffer-region-update!
         call-with-excursion
         buffer-search
         buffer-findf)

(require "rope.rkt")
(require "search.rkt")
(require "circular-list.rkt")

(require (only-in racket/string string-join))
(require (only-in racket/path normalize-path))
(require (only-in racket/file file->string))

(define main-mark-type (mark-type "main" 'right))

(struct buffergroup ([members #:mutable] ;; (CircularList Buffer)
                     ) #:transparent)

(struct buffer ([rope #:mutable]
                [pos #:mutable]
                [title #:mutable]
                [group #:mutable] ;; (Option BufferGroup)
                ) #:transparent)

(define (make-buffergroup)
  (buffergroup circular-empty))

(define (make-buffer group ;; (Option BufferGroup)
                     title ;; String
                     #:initial-contents [initial-contents ""])
  (register-buffer! group (buffer (string->rope initial-contents)
                                  0
                                  title
                                  #f)))

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
       (circular-list-memf (lambda (b) (equal? (buffer-title b) title)) (buffergroup-members group))))

(define (buffer->buffer* group b)
  (and group
       (circular-list-memf (lambda (b1) (eq? b b1)) (buffergroup-members group))))

(define (lookup-buffer group title)
  (cond [(title->buffer* group title) => circular-car] [else #f]))

(define (title-exists-in-group? group title)
  (and (title->buffer* group title) #t))

;; (Option Group) Path -> String
(define (filename->unique-buffer-title group filename)
  (define pieces (reverse (map path->string (explode-path filename))))
  (define primary-piece (car pieces))
  (define uniquifiers (cdr pieces))
  (if (not group)
      primary-piece
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
            candidate))))

(define (file->buffer group filename)
  (let* ((filename (normalize-path (simplify-path filename)))
         (title (filename->unique-buffer-title group filename))
         (b (make-buffer group title)))
    (buffer-region-update! b
                           (lambda (_dontcare) (string->rope (file->string filename)))
                           #:point 0
                           #:mark (buffer-size b))))

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

(define (buffer-move-to! buf pos)
  (set-buffer-pos! buf (max 0 (min (buffer-size buf) pos)))
  (buffer-seek! buf pos))

(define (buffer-seek! buf pos)
  (buffer-lift rope-seek buf pos))

(define (buffer-move-by! buf delta)
  (buffer-move-to! buf (+ (buffer-pos buf) delta)))

(define (buffer-mark! buf [mtype main-mark-type] #:position [pos (buffer-pos buf)] #:value [value #t])
  (buffer-lift replace-mark buf mtype pos value))

(define (buffer-clear-mark! buf [mtype main-mark-type])
  (define pos (find-mark-pos (buffer-rope buf) mtype))
  (if pos
      (buffer-lift clear-mark buf mtype pos)
      buf))

(define (buffer-mark-pos buf [mtype main-mark-type])
  (find-mark-pos (buffer-rope buf) mtype))

(define (buffer-region-split* buf pos mark)
  (define lo (min pos mark))
  (define hi (max pos mark))
  (define-values (l mr) (rope-split (buffer-rope buf) lo))
  (define-values (m r) (rope-split mr (- hi lo)))
  (values l m r))

(define (buffer-region-split buf
                             #:point [pos (buffer-pos buf)]
                             #:mark [mark (buffer-mark-pos buf)])
  (buffer-region-split* buf pos mark))

(define (buffer-region buf
                       #:point [pos (buffer-pos buf)]
                       #:mark [mark (buffer-mark-pos buf)])
  (define-values (_l m _r) (buffer-region-split* buf pos mark))
  m)

(define (buffer-region-update! buf updater
                               #:point [pos (buffer-pos buf)]
                               #:mark [mark (buffer-mark-pos buf)])
  (define-values (l m r) (buffer-region-split* buf pos mark))
  (set-buffer-rope! buf (rope-concat (list l (updater m) r)))
  buf)

(define (call-with-excursion buf f)
  (define excursion (gensym 'excursion))
  (define saved-mark-type (mark-type (format "Saved mark ~a" excursion) 'right))
  (define saved-point-type (mark-type (format "Saved point ~a" excursion) 'right))
  (buffer-mark! buf saved-mark-type #:position (buffer-mark-pos buf))
  (buffer-mark! buf saved-point-type #:position (buffer-pos buf))
  (define (restore!)
    (define restore-mark-pos (buffer-mark-pos buf saved-mark-type))
    (define restore-point-pos (buffer-mark-pos buf saved-point-type))
    (when restore-mark-pos (buffer-mark! buf #:position restore-mark-pos))
    (when restore-point-pos (buffer-move-to! buf restore-point-pos))
    (buffer-clear-mark! buf saved-mark-type)
    (buffer-clear-mark! buf saved-point-type))
  (with-handlers [(exn? (lambda (e)
                          (restore!)
                          (raise e)))]
    (define result (f))
    (restore!)
    result))

(define (buffer-search* buf start-pos forward? move? find-delta)
  (define-values (l r) (rope-split (buffer-rope buf) start-pos))
  (define delta (find-delta (if forward? r l)))
  (define new-pos (+ start-pos (cond [(not delta) 0] [forward? delta] [else (- delta)])))
  (when delta
    (if move?
        (buffer-move-to! buf new-pos)
        (buffer-seek! buf new-pos)))
  new-pos)

(define (buffer-search buf needle
                       #:position [start-pos (buffer-pos buf)]
                       #:forward? [forward? #t]
                       #:move? [move? #f])
  (buffer-search* buf start-pos forward? move?
                  (lambda (piece) (search-rope needle piece #:forward? forward?))))

(define (buffer-findf buf f
                      #:position [start-pos (buffer-pos buf)]
                      #:forward? [forward? #t]
                      #:move? [move? #f])
  (buffer-search* buf start-pos forward? move?
                  (lambda (piece) (findf-in-rope f piece #:forward? forward?))))

(define (buffer-lift f buf . args)
  (define new-rope (apply f (buffer-rope buf) args))
  (set-buffer-rope! buf new-rope)
  buf)
