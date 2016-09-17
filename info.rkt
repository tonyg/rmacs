#lang setup/infotab
(define collection 'multi)
(define deps '("base" "ansi" "syntax-color-lib"
               "gui-lib"
               "unix-signals"
               ;; this for error reporting (only):
               "web-server-lib"
               ;; plus these, needed for the tests:
               "profile-lib"
               ))
(define build-deps '("rackunit-lib"))
