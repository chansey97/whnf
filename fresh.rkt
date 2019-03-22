#lang racket
(provide freshen)

;; 2 Generating Fresh Names

(define (add-* x star)
  (string->symbol
   (string-append (symbol->string x)
                  star)))

(define (freshen used x [star "*"])
  (if (memv x used)
      (freshen used (add-* x star))
      x))

(module+ main
  (freshen '() 'x)
  ;; 'x
  (freshen '(x x*) 'x)
  ;; 'x**
  (freshen '(x y z) 'y)
  ;; 'y*

  (freshen '() 'x "$")
  ;; 'x
  (freshen '(x x*) 'x "$")
  ;; 'x$
  (freshen '(x y z) 'y "$")
  ;; 'y$
  )

