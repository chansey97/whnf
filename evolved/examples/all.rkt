#lang racket

(define example-files
  (for*/list ([d (directory-list)]
              #:when (directory-exists? d) ; dirs
              [f (in-directory d)]
              #:when (regexp-match? "\\.rkt$" f) ; files
              #:when (not (regexp-match? "\\eta.rkt$" f))) ; except files
    f))

(for ([f example-files])
  (let* ((fn1 (string-replace (path->string f) "\\" "$"))
         (fn2 (string-replace fn1 "/" "$"))
         (fn3 (string-append fn2 ".txt")))
    (with-output-to-file fn3
      (lambda () (dynamic-require f #f))
      )))
