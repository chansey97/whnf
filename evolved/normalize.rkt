#lang racket
(provide (all-defined-out))
(require "../fresh.rkt")
(require "../resugar.rkt")
(require "../pretty.rkt")
(require "./type-checker.rkt")

(struct V-EagerClos (v)
  #:transparent)
(struct V-Abs (var body) 
  #:transparent)
(struct V-Pi (var domain range)
  #:transparent)
(struct V-Add1 (n)
  #:transparent)
(struct V-Eq (type from to)
  #:transparent)

(struct N-var (x)
  #:transparent)

;; exceptions

(struct exn:fail:whnf-1-in-norm-no-rule-applies ())

;; brief-v for debugging

(define (brief-v v)
  (match v
    [(V-Clos usn env e)
     `(V-Clos ,e)]
    [(V-App u w)
     `(V-App ,(brief-v u) ,(brief-v u))]
    [(V-Ind-Nat t m b s)
     `(V-Ind-Nat ,(brief-v t) ,(brief-v m) ,(brief-v b) ,(brief-v s))]
    [(V-Replace t m b)
     `(V-Ind-Nat ,(brief-v t) ,(brief-v m) ,(brief-v b))]

    [(V-EagerClos v)
     `(V-EagerClos ,(brief-v v))]
    [(V-Abs var body)
     `(V-Abs ,(brief-v var) ,(brief-v body))]
    [(V-Pi var domain range)
     `(V-Pi ,(brief-v var) ,(brief-v domain) ,(brief-v range))]
    [(V-Add1 v)
     `(V-Add1 ,(brief-v v))]
    [(V-Eq A from to)
     `(V-Eq ,(brief-v A) ,(brief-v from) ,(brief-v to))]
    
    [_ v]    
    ))

;; whnf-1-in-norm

(define (whnf-1-in-norm v)
  ;; (printf "whnf-1-in-norm v=~v\n\n" v)
  ;; (printf "whnf-1-in-norm brief-v=~v\n\n" (brief-v v))
  (match v
    [(V-App (V-Clos usn env `(,(or 'λ 'lambda) (,x) ,b)) w)
     (V-Clos usn (extend env x (V-EagerClos w)) b)
     ;; (V-Clos usn (extend env x w) b)
     ] 
    [(V-App u w)
     (let ((v (whnf-1-in-norm u)))
       (V-App v w))]

    [(V-Ind-Nat (V-Clos usn env 'zero) m b s)
     b]
    [(V-Ind-Nat (V-Clos usn env `(add1 ,n)) m b s)
     (V-App (V-App s (V-Clos usn env n)) (V-Ind-Nat (V-Clos usn env n) m b s))]
    [(V-Ind-Nat t m b s)
     (let ((v (whnf-1-in-norm t)))
       (V-Ind-Nat v m b s))]
    
    [(V-Replace (V-Clos usn env 'same) m b)
     b]
    [(V-Replace t m b)
     (let ((v (whnf-1-in-norm t)))
       (V-Replace v m b))]
    
    [(V-Clos _ env x) #:when (var? x)
                      (let ((v (lookup x env)))
                        (match v
                          [(V-Gen x _) (N-var x)]
                          [(N-var _) v]
                          [(V-EagerClos w) w]
                          [_ v]))]
    
    ;; TODO: check why we can not force speed up whnf by (whnf-1-in-norm (V-Clos ...)) ????
    
    [(V-Clos usn env `(the ,e1 ,e2))
     ;; (whnf-1-in-norm (V-Clos usn env e2))
     (V-Clos usn env e2)
     ]

    [(V-Clos usn env `(ind-Nat ,target ,motive ,base ,step))
     ;; (whnf-1-in-norm (V-Ind-Nat (V-Clos usn env target) (V-Clos usn env motive) (V-Clos usn env base) (V-Clos usn env step)))
     (V-Ind-Nat (V-Clos usn env target) (V-Clos usn env motive) (V-Clos usn env base) (V-Clos usn env step))
     ]
    
    [(V-Clos usn env `(replace ,target ,motive ,base))
     ;; (whnf-1-in-norm (V-Replace (V-Clos usn env target) (V-Clos usn env motive) (V-Clos usn env base)))
     (V-Replace (V-Clos usn env target) (V-Clos usn env motive) (V-Clos usn env base))
     ]
    
    [(V-Clos usn env `(,e1 ,e2))
     ;; (whnf-1-in-norm (V-App (V-Clos usn env e1) (V-Clos usn env e2)))
     (V-App (V-Clos usn env e1) (V-Clos usn env e2))
     ]
    
    [none-of-the-above ; no rule apply when meet a constructor at the top
     ;; (printf "whnf-1-in-norm none-of-the-above v=~v\n" v)
     (raise (exn:fail:whnf-1-in-norm-no-rule-applies))]))

(define (eval-1 v)
  ;; (printf "eval-1 brief-v=~v\n\n" (brief-v v))
  (match v
    [(V-Abs y body)
     (V-Abs y (eval-1 body))]
    
    [(V-Pi y domain range)
     (let-values ([(domain-succ? domain-v) (with-handlers ([exn:fail:whnf-1-in-norm-no-rule-applies? (lambda (exn) (values #f domain))])
                                             (values #t (eval-1 domain)))]
                  [(range-succ? range-v) (with-handlers ([exn:fail:whnf-1-in-norm-no-rule-applies? (lambda (exn) (values #f range))])
                                           (values #t (eval-1 range)) )])
       (if (or domain-succ? range-succ?)
           (V-Pi y domain-v range-v)
           (raise (exn:fail:whnf-1-in-norm-no-rule-applies))))]
    
    [(V-Add1 n)
     (V-Add1 (eval-1 n))]
    
    [(V-Eq type from to)
     (let-values ([(type-succ? type-v) (with-handlers ([exn:fail:whnf-1-in-norm-no-rule-applies? (lambda (exn) (values #f type))])
                                         (values #t (eval-1 type)))]
                  [(from-succ? from-v) (with-handlers ([exn:fail:whnf-1-in-norm-no-rule-applies? (lambda (exn) (values #f from))])
                                         (values #t (eval-1 from)) )]
                  [(to-succ? to-v) (with-handlers ([exn:fail:whnf-1-in-norm-no-rule-applies? (lambda (exn) (values #f to))])
                                     (values #t (eval-1 to)) )])
       (if (or type-succ? from-succ? to-succ?)
           (V-Eq type-v from-v to-v)
           (raise (exn:fail:whnf-1-in-norm-no-rule-applies))))]

    ; force speed up by (eval-1 V-Abs/V-Pi/...)
    [(V-Clos usn env `(,(or 'λ 'lambda) (,x) ,b))
     (eval-1
      (let* ((y (freshen usn x))
             (neutral-y (N-var y)))
        (V-Abs y (V-Clos (cons y usn) (extend env x neutral-y) b))))]
    
    [(V-Clos usn env `(,(or 'Π 'Pi) ((,x ,a)) ,b))
     (eval-1
      (let* ((y (freshen usn x))
             (neutral-y (N-var y)))
        (V-Pi y (V-Clos usn env a) (V-Clos (cons y usn) (extend env x neutral-y) b)))) ]

    [(V-Clos usn env `(add1 ,n))
     (eval-1
      (V-Add1 (V-Clos usn env n)))]

    [(V-Clos usn env `(= ,A ,from ,to))
     (eval-1
      (V-Eq (V-Clos usn env A) (V-Clos usn env from) (V-Clos usn env to)))]

    [_ (whnf-1-in-norm v)]
    ))

;; read-back to syntax

(define (subst-in-expr used-names ρ e)
  ;; (printf "subst-in-expr enter e=~v\n" e)
  (match e
    
    [x #:when (keyword? x)
       x]
    
    [x #:when (var? x)
       (match (lookup x ρ)
         [(N-var x) x]
         [(V-Gen x k) x]
         [(V-EagerClos (V-Clos usn ρ e)) (subst-in-expr usn ρ e)]
         [_ x])]
    
    [`(,(or 'λ 'lambda) (,x) ,b) 
     (let* ((y (freshen used-names x))
            (neutral-y (N-var y)))
       `(λ (,y)
          ,(subst-in-expr (cons y used-names) (extend ρ x neutral-y) b)))]
    
    [`(,(or 'Π 'Pi) ((,x ,a)) ,b)
     (let* ((y (freshen used-names x))
            (neutral-y (N-var y)))
       `(Π ((,y ,(subst-in-expr used-names ρ a)))
            ,(subst-in-expr (cons y used-names) (extend ρ x neutral-y) b)))]

    [`(add1 ,n)
     `(add1 ,(subst-in-expr used-names ρ n))]
    
    [`(= ,A ,from ,to)
     `(= ,(subst-in-expr used-names ρ A) ,(subst-in-expr used-names ρ from) ,(subst-in-expr used-names ρ to))]

    [`(ind-Nat ,t ,m ,b ,s)
     `(ind-Nat ,(subst-in-expr used-names ρ t) ,(subst-in-expr used-names ρ m), (subst-in-expr used-names ρ b), (subst-in-expr used-names ρ s))]
    
    [`(replace ,t ,m ,b)
     `(ind-Nat ,(subst-in-expr used-names ρ t) ,(subst-in-expr used-names ρ m), (subst-in-expr used-names ρ b))]
    
    [`(the ,type ,expr)
     `(the ,(subst-in-expr used-names ρ type) ,(subst-in-expr used-names ρ expr))]
    
    [`(,e1 ,e2)
     `(,(subst-in-expr used-names ρ e1) ,(subst-in-expr used-names ρ e2))]
    ))

(define (read-back-1 v)
  (match v
    [(V-Clos usn env e) (subst-in-expr usn env e)]
    [(V-App u w) `(,(read-back-1 u) ,(read-back-1 w))]
    [(V-Ind-Nat t m b s) `(ind-Nat ,(read-back-1 t) ,(read-back-1 m) ,(read-back-1 b) ,(read-back-1 s))]
    [(V-Replace t m b) `(replace ,(read-back-1 t) ,(read-back-1 m) ,(read-back-1 b))]
    [(N-var x) x]
    [(V-Abs y body) `(λ (,y) ,(read-back-1 body))]
    [(V-Pi y domain range) `(Π ((,y ,(read-back-1 domain))) ,(read-back-1 range))]
    [(V-Add1 n) `(add1 ,(read-back-1 n))]
    [(V-Eq type from to) `(= ,(read-back-1 type) ,(read-back-1 from) ,(read-back-1 to))]
    ))

;; print steps

(define (print-1st-step v)
  (pprint-pie (read-back-1 v))
  (printf "\n\n"))

(define (print-final-step v)
  (with-handlers ([exn:fail:whnf-1-in-norm-no-rule-applies?
                   (lambda (exn)
                     (pprint-pie (read-back-1 v))
                     (printf "\n\n"))])
    (for ([i (in-naturals)])
      (set! v (eval-1 v)))))

(define (print-all-steps v)
  (pprint-pie (read-back-1 v))
  (printf "\n\n")
  (with-handlers ([exn:fail:whnf-1-in-norm-no-rule-applies? (lambda (exn) (void))])
    (for ([i (in-naturals)])
      (let* ((v2 (eval-1 v)))
        (pprint-pie (read-back-1 v2))
        (printf "\n\n")
        (set! v v2)))))

(define (print-all-steps-no-duplicate v)
  (define expr (read-back-1 v))
  (pprint-pie (resugar expr))
  (printf "\n\n")
  (with-handlers ([exn:fail:whnf-1-in-norm-no-rule-applies? (lambda (exn) (void))])
    (for ([i (in-naturals)])
      (let* ((v2 (eval-1 v))
             (expr2 (read-back-1 v2)))
        (when (not (equal? expr expr2))
          (pprint-pie (resugar expr2))
          (printf "\n\n"))
        (set! v v2)
        (set! expr expr2)))))