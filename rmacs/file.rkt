#lang racket/base

(provide gen:buffer-source
         buffer-source-title-pieces
         buffer-source-mtime
         buffer-source-read
         buffer-source-write

         (struct-out local-file-buffer-source)
         local-file-buffer-source-path)

(require racket/generic)
(require (only-in racket/file file->string))
(require (only-in racket/path normalize-path))

(define-generics buffer-source
  (buffer-source-title-pieces buffer-source)
  (buffer-source-mtime buffer-source)
  (buffer-source-read buffer-source)
  (buffer-source-write buffer-source content))

(struct local-file-buffer-source (filename)
        #:transparent
        #:methods gen:buffer-source
        [(define (buffer-source-title-pieces src)
           (reverse (map path->string (explode-path (local-file-buffer-source-path src)))))
         (define (buffer-source-mtime src)
           (file-or-directory-modify-seconds (local-file-buffer-source-path src)))
         (define (buffer-source-read src)
           (file->string (local-file-buffer-source-path src)))
         (define (buffer-source-write src content)
           (call-with-output-file (local-file-buffer-source-path src)
             (lambda (p) (write-string content p))
             #:exists 'replace))])

(define (local-file-buffer-source-path src)
  (normalize-path (simplify-path (local-file-buffer-source-filename src))))
