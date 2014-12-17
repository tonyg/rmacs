#lang setup/infotab

(define name "ansi")
(define blurb
  (list
   `(p "ANSI and VT10x escape sequences.")))
(define homepage "https://github.com/tonyg/racket-ansi")
(define primary-file "main.rkt")

(define pre-install-collection "private/install.rkt")
(define compile-omit-files '("private/install.rkt"))
