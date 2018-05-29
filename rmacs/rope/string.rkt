#lang racket/base
;; String utilities

(provide naive-string-reverse)

(define (naive-string-reverse s)
  ;; Incredibly naive. Puts combining characters *before* the thing
  ;; they combine with. Hideously wrong. Still, it works for its
  ;; intended purpose: searching for a sequence of code points in
  ;; reverse order, where the *code points* are in the wrong order
  ;; too.
  (list->string (reverse (string->list s))))
