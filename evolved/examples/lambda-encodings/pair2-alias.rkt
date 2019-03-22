#lang racket
(require "../../main.rkt")

(run-program '() 0 '() '()
             '(
               
               (define Pair2
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

               (define Pair2-alias
                 (the (Π ((AP U))
                          (Π ((BP U))
                              U))
                   Pair2))
               
               (define cons2
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((a A))
                                  (Π ((b B))
                                      ((Pair2-alias A) B)))))
                   (λ (A)
                     (λ (B)
                       (λ (a)
                         (λ (b)
                           (λ (X)
                             (λ (f)
                               ((f a) b)))))))))

               ;; (define car2
               ;;   (the (Π ((A U))
               ;;            (Π ((B U))
               ;;                (Π ((p ((Pair2-alias A) B)))
               ;;                    A)))
               ;;     (λ (A)
               ;;       (λ (B)
               ;;         (λ (p)
               ;;           ((p A)
               ;;            (λ (a)
               ;;              (λ (b) a))))))))
               
               (define car2
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((p ((Pair2-alias A) B)))
                                  A)))
                   (λ (A)
                     (λ (B)
                       TODO))))

               ;; (define car2
               ;;   (the (Π ((A U))
               ;;            (Π ((B U))
               ;;                (Π ((p ((Pair2-alias A) B)))
               ;;                    A)))
               ;;     (λ (Ae)
               ;;       (λ (Be)
               ;;         (λ (p)
               ;;           ((p Ae) TODO))))))
               
               ))
