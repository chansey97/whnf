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

               (define cons2
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((a A))
                                  (Π ((b B))
                                      ((Pair2 A) B)))))
                   (λ (A)
                     (λ (B)
                       (λ (a)
                         (λ (b)
                           (λ (X)
                             (λ (f)
                               ((f a) b)))))))))

               (define car2
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((p ((Pair2 A) B)))
                                  A)))
                   (λ (A)
                     (λ (B)
                       (λ (p)
                         ((p A)
                          (λ (a)
                            (λ (b) a))))))))

               (define cdr2
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((p ((Pair2 A) B)))
                                  B)))
                   (λ (A)
                     (λ (B)
                       (λ (p)
                         ((p B)
                          (λ (a)
                            (λ (b) b))))))))
               
               ;; (((car2 Nat) Nat) ((((cons2 Nat) Nat) zero) (add1 zero)))
               ;; (((cdr2 Nat) Nat) ((((cons2 Nat) Nat) zero) (add1 zero)))

               ;; (define flip2
               ;;   (the (Π ((A U))
               ;;            (Π ((B U))
               ;;                (Π ((p ((Pair2 A) B)))
               ;;                    ((Pair2 B) A))))
               ;;     (λ (A)
               ;;       (λ (B)
               ;;         (λ (p)
               ;;           ((((cons2 B) A) (((cdr2 A)B) p)) (((car2 A)B) p)))))))
               
               ;; (((flip2 Nat) Nat) ((((cons2 Nat) Nat) zero) (add1 zero)))

               ;; (define car2
               ;;   (the (Π ((A U))
               ;;            (Π ((B U))
               ;;                (Π ((p ((Pair2-2 A) B)))
               ;;                    A)))
               ;;     (λ (A)
               ;;       (λ (B)
               ;;         TODO))))

               ;; (define car2
               ;;   (the (Π ((A U))
               ;;            (Π ((B U))
               ;;                (Π ((p ((Pair2 A) B)))
               ;;                    A)))
               ;;     (λ (Ae)
               ;;       (λ (Be)
               ;;         (λ (p)
               ;;           ((p Ae) TODO))))))
               
               ))
