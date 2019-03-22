#lang racket
(provide (all-defined-out))
(require "../fresh.rkt")

;; reporting information

(define pie-info-hook
  (make-parameter (lambda (where what) (void))))

(define (send-pie-info where what)
  ((pie-info-hook) where what))

;; var?

(define keywords
  (list 'define
        'U
        'Nat 'zero 'add1 'ind-Nat
        'Σ 'Sigma 'cons 'car 'cdr
        'Π 'Pi 'λ 'lambda
        '= 'same 'replace
        'Trivial 'sole
        'Absurd 'ind-Absurd
        'Atom 'quote
        'the
        'TODO))

(define (keyword? x)
  (if (memv x keywords)
      #t
      #f))

(define (var? x)
  (and (symbol? x)
       (not (keyword? x))))

;; val

(struct V-Clos (usn env exp)
  #:transparent)
(struct V-Gen (x k)
  #:transparent)

(struct V-App (rator rand)
  #:transparent)
(struct V-Ind-Nat (target motive base step)
  #:transparent)
(struct V-Replace (target motive base)
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
  ;; (printf "whnf v=~v\n\n" v)
  (match v
    [(V-App (V-Clos usn env `(,(or 'λ 'lambda) (,x) ,b)) w)
     ;; (printf "whnf-1 V-App lambda=~v w=~v\n" `(lambda ,x ,b) w)
     (V-Clos usn (extend env x w) b)]
    [(V-App u w)
     (let ((v (whnf-1 u)))
       (V-App v w))]

    [(V-Ind-Nat (V-Clos usn env 'zero) m b s)
     b]
    [(V-Ind-Nat (V-Clos usn env `(add1 ,n)) m b s)
     (V-App (V-App s (V-Clos usn env n)) (V-Ind-Nat (V-Clos usn env n) m b s))]
    [(V-Ind-Nat t m b s)
     (let ((v (whnf-1 t)))
       (V-Ind-Nat v m b s))]

    [(V-Replace (V-Clos usn env 'same) m b)
     b]
    [(V-Replace t m b)
     (let ((v (whnf-1 t)))
       (V-Replace v m b))]

    [(V-Clos _ env x) #:when (var? x)
                      ;; (printf "whnf-1 x=~v env=~v \n" x env)
                      (lookup x env)]

    [(V-Clos usn env `(the ,_ ,e2))
     (V-Clos usn env e2)]

    [(V-Clos usn env `(ind-Nat ,target ,motive ,base ,step))
     (V-Ind-Nat (V-Clos usn env target) (V-Clos usn env motive) (V-Clos usn env base) (V-Clos usn env step))]
    
    [(V-Clos usn env `(replace ,target ,motive ,base))
     (V-Replace (V-Clos usn env target) (V-Clos usn env motive) (V-Clos usn env base))]
    
    [(V-Clos usn env `(,e1 ,e2)) #:when (not (keyword? e1)) ;; note: (add1 n) not a function application
                                 (V-App (V-Clos usn env e1) (V-Clos usn env e2))]
    
    ; no rule apply when meet a constructor at the top
    [none-of-the-above
     ;; (printf "whnf-1 none-of-the-above v=~v\n" v)
     (raise (exn:fail:whnf-1-no-rule-applies))]))

(define (whnf v)
  (with-handlers ([exn:fail:whnf-1-no-rule-applies?
                   (lambda (exn)
                     ;; (printf "whnf exception v=~v\n\n" v)
                     v)])
    ;; (printf "whnf enter v=~v\n" v)
    (let ((v2 (whnf-1 v)))
      ;; (printf "whnf leave v2=~v\n\n" v2)
      (whnf v2))))

;; the conversion algorithm; the integer is
;; used to represent the introduction of a fresh variable

(define (eq-val? k u1 u2)
  ;; (printf "eq-val? u1=~v u2=~v\n" u1 u2)
  (match* ((whnf u1) (whnf u2))

    [((V-Clos _ _ kw) (V-Clos _ _ kw))
     #:when (keyword? kw) ; e.g: U, Nat, zero, ...
     #t]

    [((V-Gen x1 k1) (V-Gen x2 k2))
     (= k1 k2)]

    ;; function
    [((V-App t1 w1) (V-App t2 w2))  
     (and (eq-val? k t1 t2) (eq-val? k w1 w2))]

    [((V-Clos usn1 env1 `(,(or 'λ 'lambda) (,x1) ,e1))
      (V-Clos usn2 env2 `(,(or 'λ 'lambda) (,x2) ,e2)))
     (let* ((y1 (freshen usn1 x1))
            (y2 (freshen usn2 x2))
            (neutral-y1 (V-Gen y1 k))
            (neutral-y2 (V-Gen y2 k)))
       (eq-val? (+ k 1)
                (V-Clos (cons y1 usn1) (extend env1 x1 neutral-y1) e1)
                (V-Clos (cons y2 usn2) (extend env2 x2 neutral-y2) e2)))]

    [((V-Clos usn1 env1 `(,(or 'Π 'Pi) ((,x1 ,a1)) ,b1))
      (V-Clos usn2 env2 `(,(or 'Π 'Pi) ((,x2 ,a2)) ,b2)))
     (let*((y1 (freshen usn1 x1))
           (y2 (freshen usn2 x2))
           (neutral-y1 (V-Gen y1 k))
           (neutral-y2 (V-Gen y2 k)))
       (and (eq-val? k (V-Clos usn1 env1 a1) (V-Clos usn2 env2 a2))
            (eq-val? (+ k 1)
                     (V-Clos (cons y1 usn1) (extend env1 x1 neutral-y1) b1)
                     (V-Clos (cons y1 usn2) (extend env2 x2 neutral-y2) b2))))]

    ;; Nat
    [((V-Ind-Nat t1 m1 b1 s1) (V-Ind-Nat t2 m2 b2 s2))
     (and (eq-val? k t1 t2)
          (eq-val? k m1 m2)
          (eq-val? k b1 b2)
          (eq-val? k s1 s2))]
    
    [((V-Clos usn1 env1 `(add1 ,e1))
      (V-Clos usn2 env2 `(add1 ,e2)))
     (eq-val? k (V-Clos usn1 env1 e1) (V-Clos usn2 env2 e2))]

    ;; =
    [((V-Replace t1 m1 b1) (V-Replace t2 m2 b2))
     (and (eq-val? k t1 t2)
          (eq-val? k m1 m2)
          (eq-val? k b1 b2))]
    
    [((V-Clos usn1 env1 'same) (V-Clos usn2 env2 'same))
     #t]

    [((V-Clos usn1 env1 `(= ,A1 ,from1 ,to1))
      (V-Clos usn2 env2 `(= ,A2 ,from2 ,to2)))
     (and (eq-val? k (V-Clos usn1 env1 A1) (V-Clos usn2 env2 A2))
          (eq-val? k (V-Clos usn1 env1 from1) (V-Clos usn2 env2 from2))
          (eq-val? k (V-Clos usn1 env1 to1) (V-Clos usn2 env2 to2)))]

    ;; error
    [(_ _)
     ;; (printf "eq-val? __=~v __=~v\n" u1 u2)
     #f]))

;; type-checking and type inference
(define (check-type used-names k ρ Γ e)
  (check-exp used-names k ρ Γ e (V-Clos '() '() 'U)))

(define (check-nat used-names k ρ Γ e)
  (check-exp used-names k ρ Γ e (V-Clos '() '() 'Nat)))

(define (check-exp used-names k ρ Γ e v)
  ;; (printf "check-exp e=~v v=~v\n" e v)
  (match e
    [`(,(or 'λ 'lambda) (,x) ,n)
     (match (whnf v)
       [(V-Clos usn env `(,(or 'Π 'Pi) ((,y ,a)) ,b))
        (let* ((x-new (freshen used-names x))
               (neutral-x-new (V-Gen x-new k)))
          (check-exp (cons x-new used-names)
                     (+ k 1)
                     (extend ρ x neutral-x-new)
                     (extend Γ x (V-Clos usn env a))
                     n (V-Clos (cons x-new usn) (extend env y neutral-x-new) b)))]
       [_ (error 'check-exp "~v expected Pi" e)])]
    [`(,(or 'Π 'Pi) ((,x ,a)) ,b)
     (match (whnf v)
       [(V-Clos _ _ 'U) 
        (and (check-type used-names k ρ Γ a)
             (let* ((y (freshen used-names x))
                    (neutral-y (V-Gen y k)))
               (check-type (cons y used-names)
                           (+ k 1)
                           (extend ρ x neutral-y)
                           (extend Γ x (V-Clos used-names ρ a))
                           b)))]
       [_ (error 'check-exp "~v expected U" e)])]

    ['zero
     (match (whnf v)
       [(V-Clos _ _ 'Nat)
        #t]
       [non-NAT
        (error 'check-exp "~v expected Nat" e)])]
    
    [`(add1 ,n)
     (match (whnf v)
       [(V-Clos _ _ 'Nat)
        (check-nat used-names k ρ Γ n)]
       [non-NAT
        (error 'check-exp "~v expected Nat" e)])]
    
    ['Nat
     (match (whnf v)
       [(V-Clos _ _ 'U)
        #t]
       [non-NAT
        (error 'check-exp "~v expected U" e)])]
    
    ['same
     (match (whnf v)
       [(V-Clos usn env `(= ,A ,from ,to))
        (eq-val? k (V-Clos used-names env from) (V-Clos used-names env to))]
       [non-=
        (error 'check-exp "~v expected =" e)])]

    [`(= ,A ,from ,to)
     (match (whnf v)
       [(V-Clos _ _ 'U)
        (and (check-type used-names k ρ Γ A)
             (check-exp used-names k ρ Γ from (V-Clos used-names ρ A))
             (check-exp used-names k ρ Γ to (V-Clos used-names ρ A)))]
       [_
        (error 'check-exp "~v expected U" e)])]
    
    ['TODO
     ;; (error 'check-exp "TODO expected ~v" v)
     (send-pie-info '() `(TODO ,v))
     #t
     ]
    
    [_
     ;; (printf "check-exp __ e=~v v=~v\n" e v)
     (let ((given (infer-exp used-names k ρ Γ e)))
       (if (eq-val? k given v)
           #t
           (begin
             (error 'check-exp "eq-val? expected ~v, but given ~v" v given)
             #f)))
     ]))


(define (collect-free-variables e)
  (match e
    [x #:when (var? x)
       (set x)]
    [x #:when (keyword? x)
       (set)]
    [`(,(or 'λ 'lambda) (,x) ,n)
     (set-subtract (collect-free-variables n) (set x))]
    [`(,(or 'Π 'Pi) ((,x ,a)) ,b)
     (set-union (collect-free-variables a)
                (set-subtract (collect-free-variables b) (set x)))]
    [`(add1 ,n)
     (collect-free-variables n)]

    [`(= ,A ,from ,to)
     (set-union (collect-free-variables A)
                (collect-free-variables from)
                (collect-free-variables to))]
    
    [`(ind-Nat ,target ,motive ,base ,step)
     (set-union (collect-free-variables target)
                (collect-free-variables motive)
                (collect-free-variables base)
                (collect-free-variables step))] 

    [`(replace ,target ,motive ,base)
     (set-union (collect-free-variables target)
                (collect-free-variables motive)
                (collect-free-variables base))]
    
    [`(the ,type ,expr)
     (set-union (collect-free-variables type)
                (collect-free-variables expr))]

    [`(,e1 ,e2)
     (set-union (collect-free-variables e1)
                (collect-free-variables e2))]
    ))

(define (infer-exp used-names k ρ Γ e)
  ;; (printf "infer-exp e=~v\n" e)
  (match e

    ['U
     (V-Clos '() '() 'U)]
    
    [id #:when (var? id)
        (lookup id Γ)]
    
    [`(,e1 ,e2) #:when (not (keyword? e1)) ;; note: (add1 n) not a function application
                (match (whnf (infer-exp used-names k ρ Γ e1))
                  [(V-Clos usn env `(,(or 'Π 'Pi) ((,x ,a)) ,b))
                   (if (check-exp used-names k ρ Γ e2 (V-Clos usn env a))
                       (V-Clos usn (extend env x (V-Clos used-names ρ e2)) b)
                       (error 'infer-exp "application error e2=~v a=v" e2 a))]
                  [_ (error 'infer-exp "application ~v , expected Pi" e)])]

    [`(ind-Nat ,target ,motive ,base ,step)
     (if (and (check-nat used-names k ρ Γ target)
              (check-exp used-names k ρ Γ motive (V-Clos '() '() '(Π ((n Nat)) U)))
              (check-exp used-names k ρ Γ base (V-Clos used-names ρ `(,motive zero)))
              (let* ((fv (set->list (collect-free-variables motive)))
                     (n-1 (freshen fv 'n-1 "$"))
                     (almost (freshen fv 'almost "$")))
                (check-exp used-names k ρ Γ step (V-Clos used-names ρ
                                                           `(Π ((,n-1 Nat))
                                                                (Π ((,almost (,motive ,n-1)))
                                                                    (,motive (add1 ,n-1))))))))
         (V-Clos used-names ρ `(,motive ,target))
         (error 'infer-exp "ind-Nat, failed ~v" e))
     ]

    [`(replace ,target ,motive ,base)
     (match (whnf (infer-exp used-names k ρ Γ target))
       [(V-Clos usn env `(= ,A ,from ,to))
        (if (and (check-exp used-names k ρ Γ motive (V-Clos usn env `(Π ((x ,A)) U)))
                 (check-exp used-names k ρ Γ base (V-Clos usn env `(,motive ,from))))
            (V-Clos used-names ρ `(,motive ,to))
            (error 'infer-exp "replace, failed"))]
       
       [non-EQ (error 'infer-exp "replace, expected =")])]

    [`(the ,type ,expr)
     (if (check-type used-names k ρ Γ type)
         (let ((vt (V-Clos used-names ρ type)))
           (if (check-exp used-names k ρ Γ expr vt)
               vt
               (error 'infer-exp "~v, expected ~v" expr type)))
         (error 'infer-exp "~v, expected U" type))]
    
    [_
     ;; (printf "infer-exp e=~v\n" e)
     (error 'infer-exp "cannot infer type")]))

(define (type-check m a)
  (and (check-type '() 0 '() '() a)
       (check-exp '() 0 '() '() m (V-Clos '() '() a))))
