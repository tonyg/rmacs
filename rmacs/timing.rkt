#lang racket/base

(provide time*
         profile*)

(require profile)

(define-syntax-rule (time* label exp ...)
  (let ((saved-output-port (current-output-port)))
    (parameterize ((current-output-port (current-error-port)))
      (time
       (begin0 (parameterize ((current-output-port saved-output-port)) exp ...)
         (printf "time* ~v: " label))))))

(define-syntax-rule (profile* exp ...)
  (let ((saved-output-port (current-output-port)))
    (parameterize ((current-output-port (current-error-port)))
      (profile (parameterize ((current-output-port saved-output-port)) exp ...)))))
