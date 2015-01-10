#lang racket/base
;; Syntax utilities :-(

(provide build-name)

(define (build-name id . parts)
  (datum->syntax id
                 (string->symbol
                  (apply string-append (map (lambda (p)
                                              (if (syntax? p)
                                                  (symbol->string (syntax-e p))
                                                  p))
                                            parts)))
                 id))
