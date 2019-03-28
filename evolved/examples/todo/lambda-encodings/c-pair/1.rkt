#lang racket
(require "../../../../main.rkt")

;; test TODO, type and term has different variable names
(run-program '() 0 '() '()
             '(
               (define C-Pair
                 (the (Π ((AP U))
                          (Π ((BP U))
                              U))
                   (λ (APe)
                     (λ (BPe)
                       (Π ((X U))
                           (Π ((f (Π ((a APe))
                                       (Π ((b BPe))
                                           X))))
                               X))))))

               (define c-car
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((p ((C-Pair A) B)))
                                  A)))
                   (λ (A)
                     (λ (B)
                       TODO))))
               
               ))
