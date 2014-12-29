#lang setup/infotab

(define name "editor")
(define blurb
  (list
   `(p "Emacs-like editor.")))
(define homepage "https://github.com/tonyg/racket-ansi")
(define primary-file "main.rkt")

(define racket-launcher-names '("rmacs"))
(define racket-launcher-libraries '("main.rkt"))
