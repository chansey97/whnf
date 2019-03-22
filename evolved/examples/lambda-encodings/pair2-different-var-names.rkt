#lang racket
(require "../../main.rkt")

(run-program '() 0 '() '()
             '(
               (define Pair2
                 (the (Π ((AP U))
                          (Π ((BP U))
                              U))
                   (λ (APe)
                     (λ (BPe)
                       (Π ((X U))
                           (Π ((f (Π ((a APe))
                                       (Π ((b BPe))
                                           X))))
                               X))))))

               (define car2
                 (the (Π ((A U))
                          (Π ((B U))
                              (Π ((p ((Pair2 A) B)))
                                  A)))
                   (λ (A)
                     (λ (B)
                       TODO))))

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
