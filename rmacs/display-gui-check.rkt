#lang racket/base
;; Implicitly requires display-gui.rkt if various conditions hold.

(require racket/runtime-path)

(define-runtime-path here ".")

(when (and (or (eq? (system-type) 'macosx)
      	       (eq? (system-type) 'windows)
               (getenv "DISPLAY"))
           (not (getenv "RMACS_NO_GUI")))
  (dynamic-require (build-path here "display-gui.rkt") #f))
