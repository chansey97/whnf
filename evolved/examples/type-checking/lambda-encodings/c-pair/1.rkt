#lang racket
(require "../../../../main.rkt")

;; test church Pair
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

               (define c-cons
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((a A))
                                  (Π ((b B))
                                      ((C-Pair A) B)))))
                   (λ (A)
                     (λ (B)
                       (λ (a)
                         (λ (b)
                           (λ (X)
                             (λ (f)
                               ((f a) b)))))))))

               (define c-car
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((p ((C-Pair A) B)))
                                  A)))
                   (λ (A)
                     (λ (B)
                       (λ (p)
                         ((p A)
                          (λ (a)
                            (λ (b) a))))))))

               (define c-cdr
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((p ((C-Pair A) B)))
                                  B)))
                   (λ (A)
                     (λ (B)
                       (λ (p)
                         ((p B)
                          (λ (a)
                            (λ (b) b))))))))
               
               (((c-car Nat) Nat) ((((c-cons Nat) Nat) zero) (add1 zero)))
               (((c-cdr Nat) Nat) ((((c-cons Nat) Nat) zero) (add1 zero)))

               (NORM 0)
               (NORM 2)
               ))
