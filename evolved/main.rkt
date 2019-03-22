#lang racket
(provide (all-defined-out))
(require "../fresh.rkt")
(require "./type-checker.rkt")
(require "./normalize.rkt")

(define (interact used-names k ρ Γ input)
  ;; (printf "interact input=~v\n" input)
  (match input
    [`(define ,x ,e)
     (match e
       [`(the ,type ,expr)
        (let ((_ (infer-exp used-names k ρ Γ e)) ; for checking only
              (y (freshen used-names x)))
          (values (cons y used-names)
                  k
                  (extend ρ x (V-Clos (cons y used-names) ρ expr))
                  (extend Γ x (V-Clos (cons y used-names) ρ type))))]
       [_ (error 'interact "wrong define, expected a (the type expr)")])]
    [e
     (printf "Type:\n\n")
     (print-all-steps-no-duplicate (infer-exp used-names k ρ Γ e))
     (printf "Normal form:\n\n")
     (print-all-steps-no-duplicate (V-Clos used-names ρ e))
     (values used-names k ρ Γ)
     ]))

(define (run-program used-names k ρ Γ inputs)
  (match inputs
    ['()
     ;; (values used-names k ρ Γ)
     (void)]
    
    [(cons d rest)
     (let-values ([(new-used-names new-k new-ρ new-Γ) (interact used-names k ρ Γ d)])
       ;; (printf "run-program ~v ~v ~v\n" new-k new-ρ new-Γ)
       (run-program new-used-names new-k new-ρ new-Γ rest))]))

(define (hook loc what)
  (match what
    [`(TODO ,v)
     (printf "hook TODO expect:\n")
     ;; (print-all-steps v)
     (print-all-steps-no-duplicate v)
     ]
    [_ (void)])
  (void))

(pie-info-hook hook)


