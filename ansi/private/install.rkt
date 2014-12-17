#lang racket/base

(provide pre-installer)

(require racket/file)
(require dynext/file)
(require dynext/compile)
(require dynext/link)

(define (pre-installer collections-top-path collection-path)
  (define private-path (build-path collection-path "private"))
  (parameterize ((current-directory private-path))
    (define shared-object-target-path (build-path private-path
						  "compiled"
						  "native"
						  (system-library-subpath)))
    (define shared-object-target (build-path shared-object-target-path
					     (append-extension-suffix "tty-raw-extension")))
    (when (not (file-exists? shared-object-target))
      (define c-source (build-path private-path "tty-raw-extension.c"))
      (define object (build-path shared-object-target-path "tty-raw-extension.o"))
      (make-directory* shared-object-target-path)
      (compile-extension #f ;; not quiet
                         c-source
                         object
                         '())
      (link-extension #f ;; not quiet
                      (list object)
                      shared-object-target))))
