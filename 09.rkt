#lang plai

;;;Practica9
;;Alvarez Takisawa Jose Luis
;;Palomino Garibay Alonso.

;; Data-type que representa la sintaxis abstracta de RFWAEL.
(define-type RFWAEL
  [num (n number?)]
  [bool (b boolean?)]
  [with (bds (listof bind?)) (body RFWAEL?)]
  [app (func RFWAEL?) (prs (listof RFWAEL?))]
  [ifthen (c RFWAEL?) (l RFWAEL?) (r RFWAEL?)]
  [id (name symbol?)]
  [fun (pfs (listof symbol?)) (body RFWAEL?)]
  [binop (f procedure?) (l RFWAEL?) (r RFWAEL?)]
  [isequal? (l RFWAEL?) (r RFWAEL?)])

;; validamos el ambiente
(define (Ambiente? x)(procedure? x))

;; parse-bds 
(define (parse-bds lst)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (boolean? bindRep)
        (mmap (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bds (string-append "El id " (symbol->string (car bindRep)) " esta repetido")))))



;; relacion entre el id y el value
(define-type binding
  [bind (name symbol?) (value 
                        (lambda (x) 
                                ( or  (exprV? x)(RFWAEL? x))))])


;; agregamos exprV
(define-type RFWAEL-Value
  [numV (n number?)]
  [exprV (sexp RFWAEL?) (env Ambiente?)]
  [closureV (params (listof symbol?)) (body RFWAEL?) (env Ambiente?)]
  [boolV (b boolean?)])

;; ambiete vacio
(define (emptSub)
  (lambda (name) (error 'lookup 
                        (string-append "error con el identificador '" 
                                       (symbol->string name)))))


;;Interp

(define (interp sexpr env)
  (type-case RFWAEL sexpr
      [ifthen (c tbr fbr) (let ([val-ifthen (interp c env)])
               (if (eqv? (boolV-b val-ifthen) #t)
                   (interp tbr env)
                   (interp fbr env)))][num (n) (numV n)]
    [binop (f l r) (f (strict (interp l env)) (strict (interp r env)))]      
    [fun (a b) (closureV a b env)]
    [isequal? (l r)
              (let* ((li (interp l env))
                     (ri (interp r env)))
                (cond
                  [(and (boolV? li) (boolV? ri)) (boolV (eq? (boolV-b li) (boolV-b ri)))]
                  [(and (numV? li) (numV? ri)) (boolV (eq? (numV-n li) (numV-n ri)))]
                  [else (error 'interp/isequal? "No puede comparar tipos no primitivos o distintos")]))]
    [id (v) (lookup v env)]
    [bool(n) (boolV n)]
    [app (fun-sexpr arg-sexprs)
         (let ([fun-val (strict (interp fun-sexpr env))]
               [arg-vals (mmap (lambda (arg-sexpr) (exprV arg-sexpr env)) arg-sexprs)])
           (interp (closureV-body fun-val)
                   (foldl (lambda (pf pr e)
                            (aSub pf pr e))
                          env
                          (closureV-params fun-val)
                          arg-vals)))][with (bs body) 
    (let* ((bind->aSub (lambda (b prev-env) 
    (aSub (bind-name b) 
    (interp (bind-value b) prev-env)prev-env)))
    (new-env (foldl bind->aSub env bs)))(interp body new-env))]))


;; Parser)
(define (parse sexp)
  (cond
   [(number? sexp) (num sexp)]            
   [(symbol? sexp) 
     (case sexp 
       [(true) (bool #t)]
       [(false) (bool #f)]
       [else (id sexp)])]
       [(list? sexp)
        (case (car sexp)
          [(and) (binop (lambda (a b)
                       (boolV
                       (and (boolV-b a) (boolV-b b))))
                       (parse (cadr sexp)) 
                       (parse (caddr sexp)))]
       [(or) (binop (lambda (a b)
                    (boolV
                    (or  (boolV-b b)(boolV-b a))))
                    (parse(cadr sexp))
                    (parse(caddr sexp)))]
       [(* + - /) (binop (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]                                                                                     
       [(with) (let ([bds (parse-bds (cadr sexp))])
                 (app (fun (mmap bind-name bds) (parse (caddr sexp))) 
                      (mmap bind-value bds)))]             
       [(fun) (fun (cadr sexp) (parse (caddr sexp)))]
       [(ifthen) (ifthen (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))]
       [(=) (binop (lambda (a b) 
                       (cond
                       [(and (boolV? a) (boolV? b)) (boolV (eqv? (boolV-b a)(boolV-b b)))]                                              
                       [(and (numV? b)(numV? a)) (boolV (eqv?  (numV-n b)(numV-n a)))]
                       [else  (boolV #f)])) 
                   (parse (cadr sexp)) (parse (caddr sexp)))]
       [(isequal?) (isequal? (parse (cadr sexp)) (parse (caddr sexp)))]
       [else (app (parse (car sexp)) (mmap (lambda (ex) 
                                           (parse ex))
                                         (cdr sexp)))])]
    [(boolean? sexp)(parsea-bool sexp)]))


;; operacion
(define (elige s)
  (lambda (a b) (numV 
                 ((case s [(/) /][(-) -][(+) +][(*) *])(numV-n a)(numV-n b)))))


;;lookup
(define (lookup name env)
  (env name))

;; Extiende el ambiente
(define (aSub bound-name bound-value rest-env)
  (lambda (name)
    (if (symbol=? bound-name name)
        bound-value
        (lookup name rest-env))))

;;pract6
(define (strict sexp)
  (type-case RFWAEL-Value sexp
    [exprV (e env) (strict (interp e env))]
    [else sexp]))


(define mmap (lambda (f x)
 (if (empty? x)
 (list)
 (cons (f (car x)) (mmap f (cdr x))))))


(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))


(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))



;;creamos un boolV
(define (parsea-bool sexp)
  (if(or (eqv? sexp #t)(eqv? sexp 'true))(bool false)(bool true) ))




;;Pruebas
(define-syntax-rule (read->interp p) (interp (parse p) (emptSub)))
;(read->interp '{with {{x 1} {f {fun {a b c d} {+ x {+ a {+ b {+ c d}}}}}} {y 2} {z 3} {w 4} {v 5}} {f y z w v}})
;(read->interp '{with {{x 1} {y 2}} {ifthen {isequal? {- y x} 2} 10 false}})
;(read->interp '{with {{fib {fun {x}{ifthen {= x 0}1{* x {fib {- x 1}}}}}}} {fib 5}})