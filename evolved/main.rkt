#lang racket
(provide (all-defined-out))
(require "../fresh.rkt")
(require "./type-checker.rkt")
(require "./normalize.rkt")

(define next-id 0)
(define value-by-id (make-hash))

(define (generate-id)
  (let ((id next-id))
    (set! next-id (+ 1 next-id))
    id))
(define (put id item)
  (hash-set! value-by-id id item))
(define (get id)
  (hash-ref value-by-id id))

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
     (match e
       [`(NORM ,id)
        (printf "Normalize(FNF) expression ~v:\n" id)
        (print-v-fnf (get id))
        ]
       [`(NORM-STEPS ,id)
        (printf "Normalize(FNF) expression ~v by steps:\n" id)
        (print-v-fnf-all-steps (get id))
        ]
       [_
        (let ((t (infer-exp used-names k ρ Γ e))
               (id (generate-id)))
          ;; (printf "Type(WHNF) id=~v:\n" id)
          ;; (print-v-whnf (infer-exp used-names k ρ Γ e))
          (printf "Type id=~v:\n" id)
          (print-v (infer-exp used-names k ρ Γ e))
          (put id t))
        (let ((v (V-Clos used-names ρ e))
               (id (generate-id)))
          (printf "Value(WHNF) id=~v:\n" id)
          (print-v-whnf v)
          (put id v))
        ])
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
     (let ((id (generate-id)))
       (printf "TODO expected type (id=~v):\n" id)
       (print-v v)
       (put id v))
     ]
    [_ (void)])
  (void))

(pie-info-hook hook)


