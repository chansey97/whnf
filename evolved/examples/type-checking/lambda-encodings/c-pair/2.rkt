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
               
               (define c-cons
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((a A))
                                  (Π ((b B))
                                      ((C-Pair-alias A) B)))))
                   (λ (A)
                     (λ (B)
                       (λ (a)
                         (λ (b)
                           (λ (X)
                             (λ (f1)
                               ((f1 a) b)))))))))
               
              ((((c-cons Nat) Nat) zero) (add1 zero))
              (NORM-STEPS 0)
              
               ))
