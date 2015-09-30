#lang plai

;;Alvarez Takisawa Jose Luis.
;;Garcia Rojas Adrian.
;;Palomino Garibay ALonso.


;; funcion divs que recibe un numero para obtener sus divisores
;; divs (x) -> '()
;; define (divs x)
(define (divs x)
  (divs-aux x 1))

;; funcion divs-aux que recibe un numero y un contador para generar la 
;; lista de divisores del numero recibido
;; divs-aux (x i) -> '()
;; define (divs-aux x i)
(define (divs-aux x i)
  (if (= i x) 
      (list x)
      (if(= (modulo x i) 0)
         (cons i (divs-aux x (+ i 1)))
       (divs-aux x (+ i 1)))))
 
;; funcion reduce que recibe una lista y le aplica una funcion a todos los 
;; elemntos de la lista, regresando una lista con la evaluacion.
;; reduce (<A>)-> (<B>)
;; define reduce
(define reduce
  (lambda (proc lista)
    (if (empty? (cdr lista))
        (car lista)
        (proc (car lista) (reduce proc (cdr lista)))
        )))

;; funcion que recibe dos listas y regresa listas de tamaño 2 
;; zip (<A> <B>) -> (<C>)
;; (define (zip p q)
(define (zip p q)
(cond
    ((= (count list p) 0) empty) 
    ((= (count list q) 0) empty) 
    ((> (count list p) (count list q)) (map list (acortar p q) q))
    ((> (count list q) (count list p)) (map list p (acortar q p)))
    (else (map list p q))))

;; funcion auxiliar para zip que acorta el tamaño de la lista mas grande al tamaño de la mas pequeña
;; acortar (<A> <B>) -> (<C>)
;; define (acortar x y)
(define (acortar x y)
  (if (= (count list y) 0)
      (list)
 (cons (car x) (acortar (cdr x) (cdr y)))))

;; funcion que recibe una lista y cuenta los ceros que hay en ella y su posicion relativa.
;; lee-ceros (<A>) -> (<B>)
;; (define (lee-ceros x)
(define (lee-ceros x)
  (lee-ceros-aux x 0))

;; funcion que recibe una lista y un contador para contar los ceros dentro de esta y regresa una lista 
;; con la cantidad de ceros y su posicion relativa
;; lee-ceros-aux (<x> i) -> (<B>)
;; (define (lee-ceros-aux x i)
(define (lee-ceros-aux x i)
  (if (= (length x) 0)
     (list i)
     (if ( = (car x) 0)
          (lee-ceros-aux (cdr x) (+ 1 i))
          (cons i (lee-ceros-aux (cdr x) 0)))))



;; funcion day-of-week recibe una fecha compuesta de 3 parametros año, mes y dia
;; day-of-week (x y z) -> number
;; (define (day-of-week an me di)
(define (day-of-week an me di)
  (if(and (= me 2)(= di 29)(= (modulo an 4) 0))
  (modulo (- (+ (+ (dias-año me di) (* (- an 1900) 365)) (bisiesto an)) 2) 7)
  (modulo (- (+ (+ (dias-año me di) (* (- an 1900) 365)) (bisiesto an)) 1) 7)))

;; funcion auxiliar dias-año para calcular los dias dados el mes y los dias que se le pasan como parametros
;; dias-año (x y) -> number
;; (define (dias-año mes dia)
(define (dias-año mes dia)
  (cond
    ((eq? mes 1) dia)
    ((eq? mes 2) (+ 31 dia))
    ((eq? mes 3) (+ 59 dia))
    ((eq? dia 4) (+ 90 dia))
    ((eq? mes 5) (+ 120 dia))
    ((eq? mes 6) (+ 151 dia))
    ((eq? mes 7) (+ 181 dia))
    ((eq? mes 8) (+ 212 dia))
    ((eq? mes 9) (+ 243 dia))
    ((eq? mes 10) (+ 273 dia))
    ((eq? mes 11) (+ 304 dia))
    ((eq? mes 12) (+ 334 dia))))


;; funcion auxiliar bisiesto que checa si algun año que se le pasa como parametro es bisiesto
;; bisiesto (x) -> number
;; (define (bisiesto x)
(define (bisiesto x)
  (floor (/ (- x 1900) 4)))