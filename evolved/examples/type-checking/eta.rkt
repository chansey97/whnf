#lang racket
(require "../../main.rkt")

(run-program '() 0 '() '()
             '(
               
               (define eta-expansion
                 (the (Pi ((f (Pi ((x Nat)) Nat)))
                        (= (Pi ((x Nat)) Nat) f (λ (x) (f x))))
                   (λ (f)
                     same)))

               ;; infer-exp:
               ;; '(lambda (f) same),
               ;; expected
               ;; '(Pi ((f (Pi ((x Nat)) Nat)))
               ;;    (= (Pi ((x Nat)) Nat) f (lambda (x) (f x))))
               ;;              in eq-val?
               ;; __=(V-Clos '(f) (list (cons 'f (V-Gen 'f 0))) 'f)
               ;; __=(V-Clos '(f) (list (cons 'f (V-Gen 'f 0))) '(lambda (x) (f x)))
               
               ))
