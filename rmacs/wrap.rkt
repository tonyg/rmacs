#lang racket/base

(provide (struct-out wrap)
         wrap-line-count
         basic-wrap
         buffer-lines-reverse/wrap
         buffer-lines-forward/wrap)

(require racket/generator)
(require "buffer.rkt")

(struct wrap (width ;; Nat
              points ;; (List Nat)
              eol-pos ;; Nat
              ) #:prefab)

;; Soft-wraps the line starting at sol-pos to the given width.
(define (basic-wrap buf sol-pos width)
  (define eol-pos (buffer-end-of-line buf sol-pos))
  (let loop ((soft-sol-pos sol-pos)
             (points '()))
    (define next-sol (buffer-closest-pos-for-column buf soft-sol-pos 0 width))
    (if (< next-sol eol-pos)
        (loop next-sol (cons next-sol points))
        (wrap width (reverse points) eol-pos))))

(define (wrap-line-count w)
  (+ 1 (length (wrap-points w))))

(define (buffer-lines-reverse/wrap buf pos-or-mtype wrap-fn width)
  (define start-pos (buffer-pos buf pos-or-mtype))
  (generator ()
    (let hard-break ((eol-pos (buffer-end-of-line buf start-pos)))
      (if (< eol-pos 0)
          (values #f #f)
          (let* ((sol-pos (buffer-start-of-line buf eol-pos))
                 (w (wrap-fn buf sol-pos width)))
            (let soft-break ((eol eol-pos) (ps (reverse (wrap-points w))))
              (if (null? ps)
                  (begin (yield sol-pos eol)
                         (hard-break (- sol-pos 1)))
                  (begin (when (<= (car ps) start-pos) (yield (car ps) eol))
                         (soft-break (car ps) (cdr ps))))))))))

(define (buffer-lines-forward/wrap buf pos-or-mtype wrap-fn width)
  (define start-pos (buffer-pos buf pos-or-mtype))
  (generator ()
    (let hard-break ((sol-pos (buffer-start-of-line buf start-pos)))
      (if (> sol-pos (buffer-size buf))
          (values #f #f)
          (let* ((w (wrap-fn buf sol-pos width)))
            (let soft-break ((sol sol-pos) (ps (wrap-points w)))
              (if (null? ps)
                  (begin (yield sol (wrap-eol-pos w))
                         (hard-break (+ (wrap-eol-pos w) 1)))
                  (begin (when (> (car ps) start-pos) (yield sol (car ps)))
                         (soft-break (car ps) (cdr ps))))))))))
