#lang racket
(provide (all-defined-out))
(require "../fresh.rkt")

;; var?

(define keywords
  (list 'U))

(define (keyword? x)
  (if (memv x keywords)
      #t
      #f))

(define (var? x)
  (and (symbol? x)
       (not (keyword? x))))

;; val

(struct V-Clos (env exp)
  #:transparent)
(struct V-Gen (k)
  #:transparent) 
(struct V-App (rator rand)
  #:transparent)

;; ρ & Γ

(define (extend env x v)
  (cons (cons x v) env))

(define (lookup x env)
  (match (assv x env)
    [(cons _ v) v]
    [#f (error 'lookup "Unknown variable ~v" x)] )
  )

;; exceptions
(struct exn:fail:whnf-1-no-rule-applies ())

;; whnf algorithm

(define (whnf-1 v)
  ;; (printf "whnf v=~v\n" v)
  (match v
    [(V-App (V-Clos env `(,(or 'λ 'lambda) (,x) ,body)) w)
     (V-Clos (extend env x w) body)]
    [(V-App u w)
     (let ((v (whnf-1 u)))
       (V-App v w))]
    
    [(V-Clos env x) #:when (var? x)
                    (lookup x env)]
    [(V-Clos env `(,e1 ,e2))
     (V-App (V-Clos env e1) (V-Clos env e2))]
    
    ; no rule apply when meet a constructor at the top
    [none-of-the-above
     (raise (exn:fail:whnf-1-no-rule-applies))]))

(define (whnf v)
  (with-handlers ([exn:fail:whnf-1-no-rule-applies? (lambda (exn) v)])
    ;; (printf "whnf v=~v\n" v)
    (let ((v2 (whnf-1 v)))
      (whnf v2))))

;; the conversion algorithm; the integer is
;; used to represent the introduction of a fresh variable

(define (eq-val? k u1 u2)
  (match* ((whnf u1) (whnf u2))
    
    [((V-Clos _ 'U) (V-Clos _ 'U))
     #t]
    
    [((V-Gen k1) (V-Gen k2))
     (= k1 k2)]

    [((V-App t1 w1) (V-App t2 w2))  
     (and (eq-val? k t1 t2) (eq-val? k w1 w2))]
    [((V-Clos env1 `(,(or 'λ 'lambda) (,x1) ,e1))
      (V-Clos env2 `(,(or 'λ 'lambda) (,x2) ,e2)))
     (let ((v (V-Gen k)))
       (eq-val? (+ k 1)
                (V-Clos (extend env1 x1 v) e1)
                (V-Clos (extend env2 x2 v) e2)))]
    [((V-Clos env1 `(,(or 'Π 'Pi) ((,x1 ,a1)) ,b1))
      (V-Clos env2 `(,(or 'Π 'Pi) ((,x2 ,a2)) ,b2)))
     (let ((v (V-Gen k)))
       (and (eq-val? k (V-Clos env1 a1) (V-Clos env2 a2))
            (eq-val? (+ k 1)
                     (V-Clos (extend env1 x1 v) b1)
                     (V-Clos (extend env2 x2 v) b2))))]
    
    [(_ _) #f]))

;; type-checking and type inference

(define (check-type k ρ Γ e)
  (check-exp k ρ Γ e (V-Clos '() 'U)))

(define (check-exp k ρ Γ e v)
  (match e
    [`(,(or 'λ 'lambda) (,x) ,n)
     (match (whnf v)
       [(V-Clos env `(,(or 'Π 'Pi) ((,y ,a)) ,b))
        (let ((v (V-Gen k)))
          (check-exp (+ k 1)
                     (extend ρ x v)
                     (extend Γ x (V-Clos env a))
                     n (V-Clos (extend env y v) b)))]
       [_ (error 'check-exp "expected Pi")])]
    [`(,(or 'Π 'Pi) ((,x ,a)) ,b)
     (match (whnf v)
       [(V-Clos _ 'U) 
        (and (check-type k ρ Γ a)
             (check-type (+ k 1)
                         (extend ρ x (V-Gen k))
                         (extend Γ x (V-Clos ρ a))
                         b))]
       [_ (error 'check-exp "expected U")])]
    
    [_ (eq-val? k (infer-exp k ρ Γ e) v)]))

(define (infer-exp k ρ Γ e)
  ;; (printf "infer-exp e=~v\n" e)
  (match e
    ['U
     (V-Clos '() 'U)]
    
    [id #:when (var? id)
        (lookup id Γ)]
    
    [`(,e1 ,e2)
     (match (whnf (infer-exp k ρ Γ e1))
       [(V-Clos env `(,(or 'Π 'Pi) ((,x ,a)) ,b))
        (if (check-exp k ρ Γ e2 (V-Clos env a))
            (V-Clos (extend env x (V-Clos ρ e2)) b)
            (error 'infer-exp "application error e2=~v a=v" e2 a))]
       [_ (error 'infer-exp "application, expected Pi")])]
    
    [_ (error 'infer-exp "cannot infer type")]))

(define (type-check m a)
  (and (check-type 0 '() '() a)
       (check-exp 0 '() '() m (V-Clos '() a))))

(module+ main
  (require rackunit)

  ;; ---- test type-check ----

  ;; identity
  (check-equal?
   (type-check '(λ (A)
                  (λ (x) x))
               '(Π ((A U))
                    (Π ((x A))
                        A)))
   #t)

  ;; pair
  (check-equal?
   (type-check '(λ (A)
                  (λ (B)
                    (Π ((X U))
                        (Π ((f (Π ((a A))
                                    (Π ((b B))
                                        X))))
                            X))))
               '(Π ((A U))
                    (Π ((B U))
                        U)))
   #t)

  
  ;; --- test whnf ---
  
  ;; (whnf (V-Clos '() '(Π ((A U))
  ;;                     (Π ((x A))
  ;;                       A))))

  
  )
