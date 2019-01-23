;; PL Project - Winter 2019
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; expression types
(struct var     (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num     (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool    (b)      #:transparent)  ;; a constant boolean, e.g., (boolean #f)
(struct munit   ()       #:transparent)  ;; unit value -- good for ending a list
(struct apair   (e1 e2)  #:transparent)  ;; a constant pair of two numex expressions
(struct closure (env f)  #:transparent)  ;; a closure is not in "source" programs; it is what functions evaluate to

;; mathematical functions
;; first, arithmetics
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus (e1 e2)  #:transparent)  ;; minus two expressions
(struct mult  (e1 e2)  #:transparent)  ;; mult two expressions
(struct div   (e1 e2)  #:transparent)  ;; div two expressions
;; then, logicals
(struct neg     (e)     #:transparent)  ;; negative of an expression
(struct andalso (e1 e2) #:transparent)  ;; and two expressions
(struct orelse  (e1 e2) #:transparent)  ;; or two expressions
;; modern math :D
(struct 1st (xs) #:transparent) ;; selecting the first item from an apair
(struct 2nd (xs) #:transparent) ;; selecting the second item from an apair

;; tester functions
(struct cnd     (e1 e2 e3)    #:transparent)  ;; condition on e1, resulting in e2 or e3
(struct iseq    (e1 e2)       #:transparent)  ;; checking equallity of params
(struct ifnzero (e1 e2 e3)    #:transparent)  ;; another type of condition
(struct ifleq   (e1 e2 e3 e4) #:transparent)  ;; conditioning on e1 being leq e2
(struct ismunit (e)           #:transparent)  ;; testing if an expression is munit

;; functional language specific functions :D
(struct lam     (nameopt formal body) #:transparent)  ;; a recursive(?) 1-argument function
(struct apply   (funexp actual)       #:transparent)  ;; function application
(struct with    (s e1 e2)             #:transparent)  ;; the masqueraded let function :D

;; Problem 1

(define (validate x)
  (cond
      [(var? x) (if (string? (var-string x)) x (error "Var content isn't string!"))]
      [(num? x) (if (integer? (num-int x)) x (error "Num content isn't integer!"))]
      [(bool? x) (if (boolean? (bool-b x)) x (error "Bool content isn't boolean!"))]
      [(munit? x) x]
      [(apair? x) x]
      [(closure? x) x] ;; TODO: is this correct?
      [else (error "Isn't a valid NUMEX expression.")]
  )
)

(define (racket_ex->numex_ex x)
  (cond 
    [(string? x) (var x)]
    [(integer? x) (num x)]
    [(boolean? x) (bool x)]
    [(null? x) (munit)]
    [(cons? x) (apair (car x) (cdr x))]
    [(list? x) (racketlist->numexlist x)]
    [else (error "Racket type not supported in NUMEX!")]
  )
)

(define (numex_ex->racket_ex x)
  (cond
    [(var? x) (var-string (validate x))]
    [(num? x) (num-int (validate x))]
    [(bool? x) (bool-b (validate x))]
    [(munit? x) null]
    [(apair? x) (numexlist->racketlist x)]
    [else (error "Isn't a valid NUMEX expression.")]
  )
)

; (define (racketlist->numexlist xs)
;   (cond
;     [(null? xs) (munit)]
;     [(cons? xs) (apair (racket_ex->numex_ex (car xs)) (racket_ex->numex_ex (cdr xs)))]
;     [(list? xs) (apair (racket_ex->numex_ex (car xs)) (racketlist->numexlist (cdr xs)))]
;     [else (error "Input of racketlist->numexlist must be a list or a cons!")]
;   )
; )

(define (racketlist->numexlist xs)
  (cond
    [(null? xs) (munit)]
    [(list? xs) (apair (car xs) (racketlist->numexlist (cdr xs)))]
    [(cons? xs) (apair (car xs) (cdr xs))]
    [else (error "Input of racketlist->numexlist must be a list or a cons!")]
  )
)

; (define (numexlist->racketlist xs)
;   (cond
;     [(munit? xs) null]
;     [(apair? xs) (cons (numex_ex->racket_ex (apair-e1 xs)) (numex_ex->racket_ex (apair-e2 xs)))]
;     [else (error "Input of numexlist->racketlist must be an apair!")]
;   )
; )

(define (numexlist->racketlist xs)
  (cond
    [(munit? xs) null]
    [(apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]
    [else (error "Input of numexlist->racketlist must be an apair!")]
  )
)

;; Problem 2

;; lookup a variable in an environment
(define (envlookup env str)
  (cond 
    [(null? env) (error "Unbound variable during evaluation" str)]
    [(equal? str (car (car env))) (cdr (car env))]
    [else (envlookup (cdr env) str)]
  )
)

;; checking types
(define (ct xs type_checker)
  (cond
    [(null? xs) xs]
    [(type_checker xs) xs]
    [(list? xs) (if (type_checker (car xs)) (cons (car xs) (ct (cdr xs) type_checker)) (error "Type didn't matched!" xs type_checker))]
    [(cons? xs) (if (type_checker (car xs)) (cons (car xs) (ct (cdr xs) type_checker)) (error "Type didn't matched!" xs type_checker))]
    [else (error "Input type not correct!" xs type_checker)]
  )
)

;; helping on testers
(define (if_eval_helper e1 e2 e3 env)
  (if (bool-b (ct (eval-under-env e1 env) bool?)) 
        (eval-under-env e2 env) 
        (eval-under-env e3 env)
  )
)

(define (eval-under-env e env)
  (cond 
    ;; expression types
    [(var? e) (envlookup env (var-string (validate e)))]
    [(num? e) (validate e)]
    [(bool? e) (validate e)]
    [(munit? e) e]
    [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
    [(closure? e) (validate e)]
    ;; arithmetic functions
    [(plus? e) 
        (num (+
          (num-int (ct (eval-under-env (plus-e1 e) env) num?)) 
          (num-int (ct (eval-under-env (plus-e2 e) env) num?))
        ))]
    [(minus? e) 
        (num (-
          (num-int (ct (eval-under-env (minus-e1 e) env) num?)) 
          (num-int (ct (eval-under-env (minus-e2 e) env) num?))
        ))]
    [(mult? e)
        (num (* 
          (num-int (ct (eval-under-env (mult-e1 e) env) num?)) 
          (num-int (ct (eval-under-env (mult-e2 e) env) num?))
        ))]
    [(div? e) 
        (let ([v (ct (eval-under-env (div-e2 e) env) num?)])
            (if (eq? 0 (num-int v)) (error "Division by zero!")  
              (num (quotient
                  (num-int (ct (eval-under-env (div-e1 e) env) num?)) (num-int v)
              ))
            )
        )]
    ;; logical functions
    [(neg? e)
      (let ([v (eval-under-env (neg-e e) env)])
        (cond
          [(bool? v) (bool (not (bool-b (ct v bool?))))]
          [(num? v) (num (- (num-int (ct v num?))))]
          [else (error "Type must be either bool or num!" e)]
        )  
      )]
    [(andalso? e)
        (bool (and 
          (bool-b (ct (eval-under-env (andalso-e1 e) env) bool?)) 
          (bool-b (ct (eval-under-env (andalso-e2 e) env) bool?)) 
        ))]   
    [(orelse? e) 
        (bool (or 
          (bool-b (ct (eval-under-env (orelse-e1 e) env) bool?)) 
          (bool-b (ct (eval-under-env (orelse-e2 e) env) bool?)) 
        ))]   
    ;; modern math :D
    [(1st? e) (apair-e1 (ct (eval-under-env (1st-xs e) env) apair?))]
    [(2nd? e) (apair-e2 (ct (eval-under-env (2nd-xs e) env) apair?))]
    ;; tester functions
    [(cnd? e) (if_eval_helper (cnd-e1 e) (cnd-e2 e) (cnd-e3 e) env)]
    [(iseq? e) 
        (let ([v1 (eval-under-env (iseq-e1 e) env)]
              [v2 (eval-under-env (iseq-e2 e) env)])
          (cond
            [(apair? v1)
              (if (apair? v2)
                (let ([a11 (eval-under-env (apair-e1 v1) env)] [a12 (eval-under-env (apair-e2 v1) env)]
                      [a21 (eval-under-env (apair-e1 v2) env)] [a22 (eval-under-env (apair-e2 v2) env)])
                  (bool (and
                    (equal? a11 a21)
                    (equal? a12 a22)
                  ))
                )
                (bool #f)
              )]
            [else (bool (equal? v1 v2))]
          )
        )]
    [(ifnzero? e) 
        (let ([v (if (eq? 0 (num-int (ct (eval-under-env (ifnzero-e1 e) env) num?))) (bool #f) (bool #t))]) 
            (if_eval_helper v (ifnzero-e2 e) (ifnzero-e3 e) env)
        )]
    [(ifleq? e) 
        (let ([v1 (num-int (ct (eval-under-env (ifleq-e1 e) env) num?))])
            (let ([v (if (<= v1 (num-int (ct (eval-under-env (ifleq-e2 e) env) num?))) (bool #t) (bool #f))]) 
              (if_eval_helper v (ifleq-e3 e) (ifleq-e4 e) env)
            )
        )]
    [(ismunit? e) (bool (munit? (eval-under-env (ismunit-e e) env)))]
    ;; functional language specific functions
    [(lam? e) 
        (if (and (or (string? (lam-nameopt e)) (null? (lam-nameopt e))) (string? (lam-formal e)))
             (closure env e)
             (error "NUMEX function name and parameter name must be string")
        )]
    [(apply? e)
        (let ([v (eval-under-env (apply-actual e) env)]
              [clsr (ct (eval-under-env (apply-funexp e) env) closure?)])
            (let ([clsr_fun (closure-f clsr)])
              (if (null? (lam-nameopt clsr_fun))
                (eval-under-env (lam-body clsr_fun) (cons (cons (lam-formal clsr_fun) v) (closure-env clsr)))
                (eval-under-env (lam-body clsr_fun) (cons (cons (lam-nameopt clsr_fun) clsr) 
                    (cons (cons (lam-formal clsr_fun) v) (closure-env clsr))))
              )
            )
        )]
    [(with? e) 
        (let ([s (ct (with-s e) string?)] 
              [v (eval-under-env (with-e1 e) env)])
           (eval-under-env (with-e2 e) (append (list (cons s v)) env))
        )]
    [(isbool? e) (bool (bool? (eval-under-env (isbool-b e) env)))]
    [else (error (format "bad NUMEX expression: ~v" e))]
  )
)

;; evaluating expressions
(define (eval-exp e) (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) (cnd (ismunit e1) e2 e3))

(define (with* bindings e) 
  (cond
    [(null? bindings) e]
    [else (with (caar bindings) (cdar bindings) (with* (cdr bindings) e))]
  )
)

(define (ifneq e1 e2 e3 e4) (cnd (iseq e1 e2) e4 e3))

(define (ifneq_with e1 e2 e3 e4) 
  (with "_x" e1 
    (with "_y" e2
      (ifleq (var "_y") (var "_x") 
        (ifleq (var "_x") (var "_y") e4 e3)
        e3
      )
    )
  )
)

;; Problem 4

(struct isbool (b) #:transparent)

(define map-bool
  (lam null "mapper"
    (lam "map" "xs"
      (cnd (ismunit (var "xs"))
        (munit)
        (cnd (isbool (1st (var "xs")))
          (apply (var "map") (2nd (var "xs")))
          (apair (apply (var "mapper") (1st (var "xs"))) (apply (var "map") (2nd (var "xs"))))
        )  
      )
    )
  )
)

(define numex-filter 
  (lam null "mapper" 
    (lam "map" "xs" 
      (cnd (ismunit (var "xs")) 
        (munit)
        (with "result" (apply (var "mapper") (1st (var "xs"))) 
          (ifnzero (var "result")
            (apair (var "result") (apply (var "map") (2nd (var "xs"))))
            (apply (var "map") (2nd (var "xs")))
          )
        )
      )
    )
  )
)

(define numex-all-gt
  (with "filter" numex-filter
    (lam null "i"
      (lam null "list"
        (apply 
          (apply (var "filter") (lam null "number"
            (ifleq (var "number") (var "i")
              (num 0)
              (var "number") ;; what if number was 0?
            )
          ))
          (var "list")
        )
      )
    )
  )
)

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; sorry for the name, it would have been so long otherwise :(
(define (scfv1 n e f1)
  (let ([v (compute-free-vars-helper (f1 e))])
    (cons (n (car v)) (cdr v))
  )
)
(define (scfv2 n e f1 f2)
  (let ([v1 (compute-free-vars-helper (f1 e))]
        [v2 (compute-free-vars-helper (f2 e))])
    (cons (n (car v1) (car v2)) 
      (set-union
        (cdr v1)
        (cdr v2)
      )
    )
  )
)
(define (scfv3 n e f1 f2 f3)
  (let ([v1 (compute-free-vars-helper (f1 e))]
        [v2 (compute-free-vars-helper (f2 e))]
        [v3 (compute-free-vars-helper (f3 e))])
    (cons (n (car v1) (car v2) (car v3)) 
      (set-union
        (cdr v1)
        (set-union
          (cdr v2)
          (cdr v3)
        )
      )
    )
  )
) 
(define (scfv4 n e f1 f2 f3 f4)
  (let ([v1 (compute-free-vars-helper (f1 e))]
        [v2 (compute-free-vars-helper (f2 e))]
        [v3 (compute-free-vars-helper (f3 e))]
        [v4 (compute-free-vars-helper (f4 e))])
    (cons (n (car v1) (car v2) (car v3) (car v4)) 
      (set-union
        (cdr v1)
        (set-union
          (cdr v2)
          (set-union
            (cdr v3)
            (cdr v4)
          )
        )
      )
    )
  )
)

(define (compute-free-vars e) (car (compute-free-vars-helper e)))

(define (compute-free-vars-helper e)
  (cond 
    ;; expression types
    [(var? e) (cons e (set (var-string e)))]
    [(num? e) (cons e (set))]
    [(bool? e) (cons e (set))]
    [(munit? e) (cons e (set))]
    [(apair? e) (scfv2 apair e apair-e1 apair-e2)]
    [(closure? e) (cons e (set))]
    ;; arithmetic functions
    [(plus? e) (scfv2 plus e plus-e1 plus-e2)]
    [(minus? e) (scfv2 minus e minus-e1 minus-e2)]
    [(mult? e) (scfv2 mult e mult-e1 mult-e2)]
    [(div? e) (scfv2 div e div-e1 div-e2)]
    ;; logical functions
    [(neg? e) (scfv1 neg e neg-e)]
    [(andalso? e) (scfv2 andalso e andalso-e1 andalso-e2)]
    [(orelse? e) (scfv2 orelse e orelse-e1 orelse-e2)]
    ;; modern math :D
    [(1st? e) (scfv1 1st e 1st-xs)]
    [(2nd? e) (scfv1 2nd e 2nd-xs)]
    ;; tester functions
    [(cnd? e) (scfv3 cnd e cnd-e1 cnd-e2 cnd-e3)]
    [(iseq? e) (scfv2 iseq e iseq-e1 iseq-e2)]
    [(ifnzero? e) (scfv3 ifnzero e ifnzero-e1 ifnzero-e2 ifnzero-e3)]
    [(ifleq? e) (scfv4 ifleq e ifleq-e1 ifleq-e2 ifleq-e3 ifleq-e4)]
    [(ismunit? e) (scfv1 ismunit e ismunit-e)]
    ;; functional langauge specific functions
    [(lam? e) 
        (let ([fun-free-vars (compute-free-vars-helper (lam-body e))])
          (let ([needed-free-vars (set-remove (set-remove (cdr fun-free-vars) (lam-formal e)) (lam-nameopt e))])
            (cons (fun-challenge (lam-nameopt e) (lam-formal e) (lam-body e) needed-free-vars) needed-free-vars)
          )
        )]
    [(apply? e) (scfv2 apply e apply-funexp apply-actual)]
    [(with? e) 
      (let ([v1 (compute-free-vars-helper (with-e1 e))]
            [v2 (compute-free-vars-helper (with-e2 e))]) 
        (cons (with (with-s e) (car v1) (car v2)) (set-remove (set-union (cdr v1) (cdr v2)) (with-s e)))
      )]
    [else (error (format "bad NUMEX expression: ~v" e))]
  )
)

(define (prune-fun-env env free_vars)
  (if (null? env) 
    null
    (if (set-member? free_vars (caar env))
      (cons (car env) (prune-fun-env (cdr env) free_vars))
      (prune-fun-env (cdr env) free_vars)
    )
  )
)

(define (eval-under-env-c e env)
  (cond 
    ;; expression types
    [(var? e) (envlookup env (var-string (validate e)))]
    [(num? e) (validate e)]
    [(bool? e) (validate e)]
    [(munit? e) e]
    [(apair? e) (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
    [(closure? e) (validate e)]
    ;; arithmetic functions
    [(plus? e) 
        (num (+
          (num-int (ct (eval-under-env-c (plus-e1 e) env) num?)) 
          (num-int (ct (eval-under-env-c (plus-e2 e) env) num?))
        ))]
    [(minus? e) 
        (num (-
          (num-int (ct (eval-under-env-c (minus-e1 e) env) num?)) 
          (num-int (ct (eval-under-env-c (minus-e2 e) env) num?))
        ))]
    [(mult? e)
        (num (* 
          (num-int (ct (eval-under-env-c (mult-e1 e) env) num?)) 
          (num-int (ct (eval-under-env-c (mult-e2 e) env) num?))
        ))]
    [(div? e) 
        (let ([v (ct (eval-under-env-c (div-e2 e) env) num?)])
            (if (eq? 0 (num-int v)) (error "Division by zero!")  
              (num (quotient
                  (num-int (ct (eval-under-env-c (div-e1 e) env) num?)) (num-int v)
              ))
            )
        )]
    ;; logical functions
    [(neg? e)
      (let ([v (eval-under-env-c (neg-e e) env)])
        (cond
          [(bool? v) (bool (not (bool-b (ct v bool?))))]
          [(num? v) (num (- (num-int (ct v num?))))]
          [else (error "Type must be either bool or num!" e)]
        )
      )]
    [(andalso? e)
        (bool (and 
          (bool-b (ct (eval-under-env-c (andalso-e1 e) env) bool?)) 
          (bool-b (ct (eval-under-env-c (andalso-e2 e) env) bool?)) 
        ))]   
    [(orelse? e) 
        (bool (or 
          (bool-b (ct (eval-under-env-c (orelse-e1 e) env) bool?)) 
          (bool-b (ct (eval-under-env-c (orelse-e2 e) env) bool?)) 
        ))]   
    ;; modern math :D
    [(1st? e) (apair-e1 (ct (eval-under-env-c (1st-xs e) env) apair?))]
    [(2nd? e) (apair-e2 (ct (eval-under-env-c (2nd-xs e) env) apair?))]
    ;; tester functions
    [(cnd? e) (if_eval_helper (cnd-e1 e) (cnd-e2 e) (cnd-e3 e) env)]
    [(iseq? e) 
        (let ([v1 (eval-under-env-c (iseq-e1 e) env)]
              [v2 (eval-under-env-c (iseq-e2 e) env)])
          (cond
            [(equal? v1 v2) (bool #t)]
            [else (bool #f)]
          )
        )]
    [(ifnzero? e) 
        (let ([v (if (eq? 0 (num-int (ct (eval-under-env-c (ifnzero-e1 e) env) num?))) (bool #f) (bool #t))]) 
            (if_eval_helper v (ifnzero-e2 e) (ifnzero-e3 e) env)
        )]
    [(ifleq? e) 
        (let ([v1 (num-int (ct (eval-under-env-c (ifleq-e1 e) env) num?))])
            (let ([v (if (<= v1 (num-int (ct (eval-under-env-c (ifleq-e2 e) env) num?))) (bool #t) (bool #f))]) 
              (if_eval_helper v (ifleq-e3 e) (ifleq-e4 e) env)
            )
        )]
    [(ismunit? e) (bool (munit? (eval-under-env-c (ismunit-e e) env)))]
    ;; functional langauge specific functions
    [(fun-challenge? e)
        (if (and 
          (or (string? (fun-challenge-nameopt e)) (null? (fun-challenge-nameopt e)))
          (string? (fun-challenge-formal e)))
            (closure (prune-fun-env env (fun-challenge-freevars e)) e)
            (error "NUMEX function name and parameter name must be string")
        )]
    [(apply? e) 
        (let ([v (eval-under-env-c (apply-actual e) env)]
              [clsr (eval-under-env-c (apply-funexp e) env)])
              (if (null? (lam-nameopt (apply-funexp e)))
                (eval-under-env-c (fun-challenge-body (apply-funexp e)) (cons (cons (fun-challenge-formal (apply-funexp e)) v) (closure-env clsr)))
                (eval-under-env-c (fun-challenge-body (apply-funexp e)) (cons (cons (fun-challenge-nameopt (apply-funexp e)) clsr) 
                    (cons (cons (fun-challenge-formal (apply-funexp e)) v) (closure-env clsr))))
              )
        )]
    [(with? e) 
        (let ([s (ct (with-s e) string?)] 
              [v (eval-under-env-c (with-e1 e) env)])
           (eval-under-env-c (with-e2 e) (append (list (cons s v)) env))
        )]
    [else (error (format "bad NUMEX expression: ~v" e))]
  )
)
;; Do NOT change this
(define (eval-exp-c e) (eval-under-env-c (compute-free-vars e) null))