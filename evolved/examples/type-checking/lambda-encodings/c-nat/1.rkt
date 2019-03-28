#lang racket
(require "../../../../main.rkt")

;; test church Nat 0 1 2 3 ...
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

               c-1
               c-2
               c-3
               (NORM 0)
               (NORM 1)
               (NORM 3)
               (NORM 5)
               
               ))
