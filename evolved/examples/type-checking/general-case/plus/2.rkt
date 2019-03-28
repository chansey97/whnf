#lang racket
(require "../../../../main.rkt")

;; test + by smaller definitions
(run-program '() 0 '() '()
             '(

               (define mot-+
                 (the (Π ((k Nat))
                          U)
                   (λ (k)
                     Nat)))
               
               (define step-+
                 (the (Π ((n-1 Nat))
                          (Π ((+_n-1 Nat))
                              Nat))
                   (λ (n-1)
                     (λ (+_n-1)
                       (add1 +_n-1)))))
               
               (define +
                 (the (Π ((n Nat))
                          (Π ((j Nat))
                              Nat))
                   (λ (n)
                     (λ (j)
                       (ind-Nat n
                         mot-+
                         j
                         step-+)))))

               ;; (+ 2 3)
               ((+ (add1 (add1 zero))) (add1 (add1 (add1 zero))))

               (NORM 0)
               (NORM 1)
               
               (NORM-STEPS 0)
               (NORM-STEPS 1)

               ))
