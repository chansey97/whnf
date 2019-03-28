#lang racket
(require "../../../../main.rkt")

;; test church iter-Nat
(run-program '() 0 '() '()
             '(

               (define C-Nat
                 (the U
                   (Π ((A U))
                       (Π ((s (Π ((so-far A))
                                   A)))
                           (Π ((b A))
                               A)))))
               
               (define c-zero
                 (the C-Nat
                   (λ (A)
                     (λ (s)
                       (λ (b)
                         b)))))

               (define c-add1
                 (the (Pi ((n C-Nat))
                        C-Nat)
                   (λ (n)
                     (λ (A)
                       (λ (s)
                         (λ (b)
                           (s (((n A) s) b))))))))

               (define c-1
                 (the C-Nat (c-add1 c-zero)))
               (define c-2
                 (the C-Nat (c-add1 (c-add1 c-zero))))
               (define c-3
                 (the C-Nat (c-add1 (c-add1 (c-add1 c-zero)))))

               (define iter-C-Nat
                 (the (Pi ((B U))
                        (Pi ((t C-Nat))
                          (Pi ((b B))
                            (Pi ((s (Pi ((so-far B))
                                      B)))
                              B))))
                   (λ (B)
                     (λ (t)
                       (λ (b)
                         (λ (s)
                           (((t B) s) b)))))))

               ((((iter-C-Nat Nat)
                  c-3)
                 zero)
                (λ (so-far)
                  (add1 so-far)))

               (NORM 1)
               (NORM-STEPS 1)
               ))
