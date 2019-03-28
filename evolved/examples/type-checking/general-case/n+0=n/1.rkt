#lang racket
(require "../../../../main.rkt")

;; test n+0=n
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

               (define n+0=n
                 (the (Π ((n Nat))
                          (= Nat ((+ n) zero) n))
                   (λ (n)
                     (ind-Nat n
                       (the (Π ((k Nat))
                                U)
                         (λ (k)
                           (= Nat ((+ k) zero) k)))
                       same 
                       (the (Π ((n-1 Nat))
                                (Π ((n+0=n_n-1 (= Nat ((+ n-1) zero) n-1)))
                                    (= Nat ((+ (add1 n-1)) zero) (add1 n-1))))
                         (λ (n-1)
                           (λ (n+0=n_n-1)
                             (replace n+0=n_n-1
                               (the (Π ((k Nat))
                                        U)
                                 (λ (k)
                                   (= Nat (add1 ((+ n-1) zero)) (add1 k))))
                               same))))))))

               
               (n+0=n (add1 (add1 zero)))
               
               (NORM 0)
               (NORM 1)
               
               (NORM-STEPS 0)
               (NORM-STEPS 1)
               ))