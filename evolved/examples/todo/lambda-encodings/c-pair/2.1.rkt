#lang racket
(require "../../../../main.rkt")

;; test type alias
(run-program '() 0 '() '()
             '(
               
               (define C-Pair
                 (the (Π ((A U))
                          (Π ((B U))
                              U))
                   (λ (A)
                     (λ (B)
                       (Π ((X U))
                           (Π ((f (Π ((a A))
                                       (Π ((b B))
                                           X))))
                               X))))))

               (define C-Pair-alias
                 (the (Π ((A U))
                          (Π ((B U))
                              U))
                   C-Pair))

               (define car2
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((p ((C-Pair-alias A) B)))
                                  A)))
                   (λ (A)
                     (λ (B)
                       (λ (p)
                         ((p A) TODO))))))
               
               ))
