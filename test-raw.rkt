#lang racket/base

(require "tty-raw-extension")

(define (main)
  (tty-raw!)
  (let loop ()
    (define ch (read-byte))
    (printf "Byte: ~v\015\012" ch)
    ;;(flush-output)
    (if (member ch '(4 8 127)) ;; EOF, C-h, DEL
	(exit)
	(loop))))

(main)
