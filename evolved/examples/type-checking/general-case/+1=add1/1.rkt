#lang racket
(require "../../../../main.rkt")

;; test +1=add1
(run-program '() 0 '() '()
             '( 
               (define +
                 (the (Π ((n Nat))
                          (Π ((j Nat))
                              Nat))
                   (λ (n)
                     (λ (j)
                       (ind-Nat n
                         (the (Π ((k Nat))
                                  U)
                           (λ (k)
                             Nat))
                         j
                         (the (Π ((n-1 Nat))
                                  (Π ((+_n-1 Nat))
                                      Nat))
                           (λ (n-1)
                             (λ (+_n-1)
                               (add1 +_n-1)))))))))

               (define +1=add1
                 (the (Π ((n Nat))
                          (= Nat ((+ (add1 zero)) n) (add1 n)))
                   (λ (n)
                     same)))

               (+1=add1 (add1 (add1 zero)))

               (NORM 0)
               (NORM 1)
               
               (NORM-STEPS 0)
               (NORM-STEPS 1)
               
               ))
