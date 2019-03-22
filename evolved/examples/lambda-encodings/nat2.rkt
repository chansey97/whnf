#lang racket
(require "../../main.rkt")

(run-program '() 0 '() '()
             '(

               (define Nat2
                 (the U
                   (Π ((A U))
                       (Π ((s (Π ((so-far A))
                                   A)))
                           (Π ((b A))
                               A)))))
               
               (define c0
                 (the Nat2
                   (λ (A)
                     (λ (s)
                       (λ (b)
                         b)))))

               (define scc
                 (the (Pi ((n Nat2))
                        Nat2)
                   (λ (n)
                     (λ (A)
                       (λ (s)
                         (λ (b)
                           (s (((n A) s) b))))))))
                              
               ;; (scc c0)

               (define c1
                 (the Nat2 (scc c0)))
               (define c2
                 (the Nat2 (scc (scc c0))))
               (define c3
                 (the Nat2 (scc (scc (scc c0)))))

               ;; The following definition is wrong:
               ;; (define iter-Nat2
               ;;   (the (Pi ((B U))
               ;;          (Pi ((t (Nat2 B)))
               ;;            (Pi ((b B))
               ;;              (Pi ((s (Pi ((so-far B))
               ;;                        B)))
               ;;                B))))
               ;;     (λ (B)
               ;;       (λ (t)
               ;;         (λ (b)
               ;;           (λ (s)
               ;;             ((t s) b)))))))

               ;; ((((iter-Nat2 Nat)
               ;;    (c3 Nat))
               ;;   zero)
               ;;  (λ (so-far)
               ;;    (add1 so-far)))

               ;; infer-exp: application '(Nat2 B) , expected Pi
               ;; Nat2 self is Pi, we can only call to lambda term, not Pi term! 

               ;; This is correct:
               (define iter-Nat2
                 (the (Pi ((B U))
                        (Pi ((t Nat2))
                          (Pi ((b B))
                            (Pi ((s (Pi ((so-far B))
                                      B)))
                              B))))
                   (λ (B)
                     (λ (t)
                       (λ (b)
                         (λ (s)
                           (((t B) s) b)))))))

               ((((iter-Nat2 Nat)
                  c3)
                 zero)
                (λ (so-far)
                  (add1 so-far)))

               ))
