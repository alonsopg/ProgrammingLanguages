#lang plai
;;Alvarez Takisawa Jose Luis.
;;Palomino Garibay Alonso.
;##################INTERP############################################

;; Agregamos exprV
(define-type LFWAE-Value
  [numV (n number?)]
  [closureV (params (listof symbol?)) (body LFWAE?) (env Env?)]
  [exprV (exp LFWAE?) (env Env?)]
  [boolV (b boolean?)])


(define-type Env
  [mtSub]
  [aSub (name symbol?) (value LFWAE-Value?) (env Env?)])

(define (lookup x env)
  (type-case Env env
    [aSub (n v e) (if (eq? x n) v (lookup x e))]
    [mtSub () (error 'lookup "Variable libre")]))

(define (interp exp env)
  (type-case LFWAE exp
  [ifthen (c tbr fbr)
            (let ((cond-val (interp c env)))
              (if (and (boolV? cond-val) (boolV-b cond-val))
                  (interp tbr env)
                  (interp fbr env)))]
    [isequal? (l r)
              (let* ((li (interp l env))
                     (ri (interp r env)))
                (cond
                  [(and (numV? li) (numV? ri)) (boolV (eq? (numV-n li) (numV-n ri)))]
                  [(and (boolV? li) (boolV? ri)) (boolV (eq? (boolV-b li) (boolV-b ri)))]
                  [else (error 'interp/isequal? "No puede comparar tipos no primitivos o distintos")]))]
    [num (n) (numV n)]    
    [bool(n) (boolV n)]                                   
    [binop (op l r) (op (strict (interp l env)) (strict (interp r env)))]       
    [id (x) (lookup x env)]
     [rec (bds b-b)
      (interp b-b
              (foldl (lambda (bndg c)
                       (letrec ([r-e (lambda (v)
                                               (if (symbol=? v (bind-name bndg))
                                                   (exprV  (bind-val bndg) r-e)
                                                   (lookup v c)))])
                         r-e))
                     env
                     bds))]
    [fun (params body) (closureV params body env)]
    [with (bs body) 
          (let* ((bind->aSub (lambda (b prev-env) 
                               (aSub (bind-name b) 
                                     (interp (bind-val b) prev-env) 
                                     prev-env)))
                 (new-env (foldl bind->aSub env bs)))
            (interp body new-env))]    
   [app (f args)
         (let ([fun-val (strict (interp f env))]
               [arg-vals (mmap (lambda (f-arg) (exprV f-arg env)) args)])
           (interp (closureV-body fun-val)
                   (foldl (lambda (pf pr e)
                            (aSub pf pr e))
                          env
                          (closureV-params fun-val)
                          arg-vals)))]
   ))


(define-syntax-rule (read->interp p) (interp (parse (lexer p)) (mtSub)))
;(read->interp "{with {{x 1} {f {fun {a b c d} {+ x {+ a {+ b {+ c d}}}}}} {y 2} {z 3} {w 4} {v 5}} {f y z w v}}")
;(read->interp "{with {{x 1} {y 2}} {ifthen {isequal? {- y x} 2} 10 false}}")

;##################INTERP############################################

;##################BASE##############################################
;; LFWAE con multi-with.
;; <LFWAE> ::= <id>
;;          | <num>
;;          | {<binop> <LFWAE> <LFWAE>}
;;          | {with {[<id> <LFWAE>]...} <LFWAE>}
;;          | {fun {<id>} <LFWAE>}
;;          | {<LFWAE> <lFWAE>*}
;; op ::= +|-|*|/
(define-type LFWAE
  [num (n number?)]
  [bool (b boolean?)]
  [with (bindings (listof bind?)) (body LFWAE?)]
  [id (name symbol?)]
  [rec (bindings (listof bind?)) (body LFWAE?)]
  [fun (pfs (listof symbol?)) (body LFWAE?)]
  [app (func LFWAE?) (prs (listof LFWAE?))]
  [binop (f procedure?) (l LFWAE?) (r LFWAE?)]
  [ifthen (c LFWAE?) (l LFWAE?) (r LFWAE?)]
  [isequal? (l LFWAE?) (r LFWAE?)])

;; definimmos el data-type binding
(define-type binding
  [bind (name symbol?) (val (lambda (bds) (or (LFWAE? bds) (exprV? bds))))])

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (boolean? bindRep)
        (mmap (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " estÃ¡ repetido")))))

;; Esta función existe para redefinir algún operador de nuestro lenguaje LFWAE
;; en una función primitiva de Racket.Tambien manejamos aqui el operador que ya maneja 
;; valores de nuestro lenguaje LFWAE-Values
(define (elige s)
  (lambda (a b) (numV 
                 ((case s
                   [(+) +]
                   [(-) -]
                   [(*) *]
                   [(/) /])
                 (numV-n a) 
                 (numV-n b)))))

;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> RFWAE
;; Toma una expresión en sintaxis concreta y genera el árbol de sintaxis abstracta.
;; (define (parse sexp)
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]   
    [(boolean? sexp)(parsea-bool sexp)]
     [(symbol? sexp) 
     (case sexp 
       [(true) (bool #t)]
       [(false) (bool #f)]
       [else (id sexp)])]    
    [(list? sexp)
     (case (car sexp)
       [(+ - * /) (binop (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(and) (binop (lambda (a b)
                       (boolV
                       (and (boolV-b a) (boolV-b b))))
                       (parse (cadr sexp)) 
                       (parse (caddr sexp)))]
       [(or) (binop (lambda (a b)
                    (boolV
                    (or (boolV-b a) (boolV-b b))))
                    (parse(cadr sexp))
                    (parse(caddr sexp)))]                                                                                  
       [(with) (let ([bds (parse-bindings (cadr sexp))])
                 (app (fun (mmap bind-name bds) (parse (caddr sexp))) 
                      (mmap bind-val bds)))]       
       [(rec) (rec (parse-bindings (second sexp)) (parse (caddr sexp)))]
       [(fun) (fun (cadr sexp) (parse (caddr sexp)))]                     
       [(ifthen) (ifthen (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))]
       [(isequal?) (isequal? (parse (cadr sexp)) (parse (caddr sexp)))]                                   
       [else (app (parse (car sexp)) (mmap parse (cdr sexp)))])]))


;##################BASE##############################################



;##################LEXER#############################################
;; B::= {|}|<number>|<symbol>
;; toks listof(<char>) listof(<char>) -> listof(B)
;; Toma una cadena de caractéres y genera una lista de llaves que abren, cierran, números
;; y símbolos.
;; (define (toks l acc)
(define (toks l acc)
  (if (empty? l)
      (if (empty? acc)
          empty
          (let* ((taux (list->string (reverse acc)))
                 (nm (string->number taux))
                 (sm (string->symbol taux))
                 (t (if nm nm sm)))
            t))
      (let* ((c (car l))) 
        (cond
          ((or (eq? c #\{)
               (eq? c #\})) (if (empty? acc)
                                (cons c (toks (cdr l) empty))
                                (let* ((taux (list->string (reverse acc)))
                                       (nm (string->number taux))
                                       (sm (string->symbol taux))
                                       (t (if nm nm sm)))
                                  (cons t (cons c (toks (cdr l) empty))))))
          ((eq? c #\ ) (if (empty? acc)
                           (toks (cdr l) empty)
                           (let* ((taux (list->string (reverse acc)))
                                  (nm (string->number taux))
                                  (sm (string->symbol taux))
                                  (t (if nm nm sm)))
                             (cons t (toks (cdr l) empty)))))
          (else (toks (cdr l) (cons c acc)))))))

;; A::= <number>|<symbol>|listof(<A>)
;; B::= {|}|<number>|<symbol>|
;; until-next-end-brace: listof(B) -> (listof(A), listof(B))
;; Determina los numeros, símbolos o listas hasta la llave que cierra al ambiente
;; dada una lista de llaves que abren, cierran, números y símbolos.
(define (until-next-end-brace l)
  (if (empty? l)
      (list empty empty)
      (let ((t (car l))) 
        (cond
          ((eq? t #\{) (let* ((ds (until-next-end-brace (cdr l)))
                              (tks (car ds))
                              (rem (cadr ds))) 
                         (if (empty? rem)
                             tks
                             (let* ((new-ds (until-next-end-brace rem))
                                    (new-tks (car new-ds))
                                    (new-rem (cadr new-ds)))
                               (list (cons tks new-tks) new-rem)))))
          ((eq? t #\}) (list empty (cdr l)))
          (else (let* ((ds (until-next-end-brace (cdr l)))
                       (tks (car ds))
                       (rem (cadr ds)))
                  (list (cons t tks) rem)))))))

;; A::= <number>|<symbol>|listof(<A>)
;; lexer: <string> -> A
;; Toma una cadena de caractéres y genera una expresión en sintaxis concreta.
;; (define (lexer s)
(define (lexer s)
  (let ((ts (toks (string->list s) empty)))
    (if (list? ts)
        (until-next-end-brace ts)
        ts)))
;##################LEXER#############################################

(define mmap (lambda (f x)
 (if (empty? x)
 (list)
 (cons (f (car x)) (mmap f (cdr x))))))

;;Funcion que nos ayuda a determinar un boolV
(define (parsea-bool exp)
  (if(or (eqv? exp #t)(eqv? exp 'true))
     (bool true)
     (bool false) ))

;; funcion strict que genere la evaluacion final de una expresion.
(define (strict exp)
  (type-case LFWAE-Value exp
    [exprV (e env) (strict (interp e env))]
    [else exp]))


