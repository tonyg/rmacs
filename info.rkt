#lang setup/infotab
(define collection 'multi)
(define deps '("base" "ansi"
               ;; this for error reporting (only):
               "web-server-lib"
               ;; plus these, needed for the tests:
               "profile-lib"
               ))
(define build-deps '("rackunit-lib"))
