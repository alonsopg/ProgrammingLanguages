#lang plai

(require 2htdp/image)
(require "p3b-base.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.- a)
;;preorderAB hace los recorridos del arbol en preorden
;;(ArbolB)-> (list)
;;(define (preorderAB arb)
(define (preorderAB arb)
 (type-case ArbolB arb
  [HVaciaB () empty]
  [NodoB (nds l v r)
    (cond
    [(and (HVaciaB? l)(HVaciaB? r)) (list v)]
    [(HVaciaB? l) (append (list v) (preorderAB r))]
    [(HVaciaB? r) (append (list v) (preorderAB l))]
    (else (append (list v) (preorderAB l) (preorderAB r))))]))
;(test (preorderAB arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.- b)
;;inorderAB hace los recorridos del arbol en inorden
;;(ArbolAB)->(list)
;; (define (inorderAB arb)
(define (inorderAB arb)
 (type-case ArbolB arb
  [HVaciaB () empty]
  [NodoB (nds l v r)
    (cond
    [(and (HVaciaB? l)(HVaciaB? r)) (list v)]
    [(HVaciaB? l) (append (list v) (inorderAB r))]
    [(HVaciaB? r) (append (inorderAB l) (list v))]
    (else (append (inorderAB l) (list v) (inorderAB r))))]))
;(test (inorderAB arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.- c)
;;posorderAB hace los recorridos del arbol en posorden.
;;(ArbolAB) -> (list)
;;(define (posorderAB arb)
(define (posorderAB arb)
 (type-case ArbolB arb
  [HVaciaB () empty]
  [NodoB (nds l v r)
    (cond
    [(and (HVaciaB? l)(HVaciaB? r)) (list v)]
    [(HVaciaB? l) (append (posorderAB r) (list v))]
    [(HVaciaB? r) (append (posorderAB l) (list v))]
    (else (append (posorderAB l) (posorderAB r) (list v))))]))
;(test (posorderAB arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2.- a)
;;Dado un arbol de tipo ArbolB y un valor numerico, agregarlo al arbol de forma ordenada.
;; (ArbolAb) -> (ArbolAB)
;;(define (insertnAB arb v)
(define (insertnAB arb v)
  (type-case ArbolB arb
  [HVaciaB () (NodoB < (HVaciaB) v (HVaciaB))]
  [NodoB (ndn l v1 r)
    (cond
    [(and (HVaciaB? l)(HVaciaB? r)) (if(ndn v1 v)
                                       (NodoB ndn (HVaciaB) v1 (NodoB ndn (HVaciaB) v (HVaciaB)))
                                       (NodoB ndn (NodoB ndn (HVaciaB) v (HVaciaB)) v1 (HVaciaB)))]
    [(HVaciaB? l) (if (ndn v1 v)
                      (NodoB ndn (HVaciaB) v1 (insertnAB r v))
                      (NodoB ndn (NodoB ndn (HVaciaB) v (HVaciaB)) v1 r))]
    [(HVaciaB? r) (if (ndn v1 v)
                      (NodoB ndn l v1 (NodoB ndn (HVaciaB) v (HVaciaB)))
                      (NodoB ndn (insertnAB l v) v1 r))]
    (else (if (ndn v1 v)
              (NodoB ndn l v1 (insertnAB r v))
              (NodoB ndn (insertnAB l v) v1 r))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2.- b)
;; insertsAB Dado un arbol de tipo ArbolB y un string, agregarlo al arbol de forma ordenada
;;(ArbolAb) -> (ArbolAB)
;;(define (insertsAB arb v)
(define (insertsAB arb v)
  (type-case ArbolB arb
  [HVaciaB () (NodoB string<? (HVaciaB) v (HVaciaB))]
  [NodoB (nds l v1 r)
    (cond
    [(and (HVaciaB? l)(HVaciaB? r)) (if(nds v1 v)
                                       (NodoB nds (HVaciaB) v1 (NodoB nds (HVaciaB) v (HVaciaB)))
                                       (NodoB nds (NodoB nds (HVaciaB) v (HVaciaB)) v1 (HVaciaB)))]
    [(HVaciaB? l) (if (nds v1 v)
                      (NodoB nds (HVaciaB) v1 (insertsAB r v))
                      (NodoB nds (NodoB nds (HVaciaB) v (HVaciaB)) v1 r))]
    [(HVaciaB? r) (if (nds v1 v)
                      (NodoB nds l v1 (NodoB nds (HVaciaB) v (HVaciaB)))
                      (NodoB nds (insertsAB l v) v1 r))]
    (else (if (nds v1 v)
              (NodoB nds l v1 (insertsAB r v))
              (NodoB nds (insertsAB l v) v1 r))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2.- c)
;;searchAB Dado un arbol de tipo ArbolB y un valor, regresar si se encuentra dicho valor en el arbol
;; (ArbolB)->(bool)
;;(define (searchAB arb v)
(define (searchAB arb v)
  (type-case ArbolB arb
  [HVaciaB () (#f)]
  [NodoB (c l v1 r) 
    (cond
    [(and (HVaciaB? l)(HVaciaB? r)) (if (eq? v v1) #t #f)]
    [(HVaciaB? l) (if (eq? v v1) #t (searchAB r v))]
    [(HVaciaB? r) (if (eq? v v1) #t (searchAB l v))]
    (else (if (eq? v v1) #t (if (string<? (~v v1) (~v v))
                                (searchAB r v)
                                (searchAB l v)))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2.- d)
;;rotateAB Dado un arbol de tipo ArbolB y una bandera booleana, rotar el arbol a la izquierda si
;;la bandera es true, o a la derecha si es false.
;;(ArbolB) -> (ArbolB)
;;(define (rotateAB arb b)
(define (rotateAB arb b)
  (type-case ArbolB arb
  [HVaciaB () HVaciaB]
  [NodoB (c l v r) 
    (cond
    [(and (HVaciaB? l)(HVaciaB? r)) (NodoB c l v r)]
    [(HVaciaB? l) (if (eq? #t b) (rotaL l r v) (NodoB c l v r))]
    [(HVaciaB? r) (if (eq? #t b) (NodoB c l v r) (rotaR r l v))]
    (else (if (eq? #t b) (rotaL l r v) (rotaR r l v))))]))

(define (rotaL l arb v)
  (type-case ArbolB arb
    [HVaciaB () ""]
    [NodoB (c lH v1 rH) 
         (NodoB c (NodoB c l v lH) v1 rH)]))

(define (rotaR r arb v)
  (type-case ArbolB arb
    [HVaciaB () ""]
    [NodoB (c lH v1 rH) 
         (NodoB c lH v1 (NodoB c rH v r))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2.- e)

#|(define (deleteAB arb v)
  (type-case ArbolB arb
  [HVaciaB () (HVaciaB)]
  [NodoB (c l v1 r)
         (cond
         [(eq? v1 v)
                             (cond
                               [(and (HVaciaB? l) (HVaciaB? r)) HVaciaB]
                               [(HVaciaB? l) (deleteAB (rotateAB arb #t) v)]
                               [(HVaciaB? r) (deleteAB (rotateAB arb #f) v)]
                               (else (deleteAB (rotateAB arb #f) v)))]
         [(and (HVaciaB? l) (HVaciaB? r)) (NodoB c l v1 r)]
         [(string<? (~v v1) (~v v)) 
                                      
                                     (NodoB c l v1 (deleteAB r v))]
         [(string<? (~v v) (~v v1)) 
                                      
                                    (NodoB c (deleteAB l v) v1 r)]
         (else arb))]))|#


(define (deleteAB arb v)
  (type-case ArbolB arb
    [HVaciaB () (HVaciaB)]
    [NodoB (c l v1 r)
           (cond
             [(and (HVaciaB? l) (HVaciaB? r)) HVaciaB]
             [(HVaciaB? l) (deleteAB (NodoB r) v)]
             [(HVaciaB? r) (deleteAB (NodoB l) v)]
             [else (deleteAB (v (NodoB r))
                             (v (NodoB r))
                             (NodoB l)
                             (deleteAB (NodoB r)))])]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3.- a)
;;Dada una funcion de aridad 2, un elemento base y una lista de elementos,
;;regresar el resultado de aplicar la funcion a la base y el primer elemento.
;;(fun exp lst) -> (exp)
;;(define (reduce-with-base func exp lst)
(define (reduce-with-base func exp lst)
 (if (null? lst)
     exp
     (reduce-with-base func (func exp (car lst)) (cdr lst))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3.- b)
;;Dada una funcion de aridad 2, un elemento base y una lista de ele-mentos, regresa la lista de resultados de aplicar la reduccion, 
;;empezando por la primera base, incluyendolos resultados intermedios y terminando con la reduccion final
;;(fun exp lst) -> (lst)
;;(define (reduce-with-base-as-list func exp lst) 
(define (reduce-with-base-as-list func exp lst) 
  (if (null? lst)
      (list exp)
      (append (list exp) (reduce-with-base-as-list func (func exp (car lst)) (cdr lst)))))

;;(define maxinsorted (reduce-with-base-as-list insertnAB vc (list 6 3 2 7)))
;;(define maxissorted (reduce-with-base-as-list insertsAB vc (list "jdbc" "cdr" "foo")))

;(test maxinsorted 
;      (list
; (HVaciaB)
; (NodoB < (HVaciaB) 6 (HVaciaB))
; (NodoB < (NodoB < (HVaciaB) 3 (HVaciaB)) 6 (HVaciaB))
; (NodoB < (NodoB < (NodoB < (HVaciaB) 2 (HVaciaB)) 3 (HVaciaB)) 6 (HVaciaB))
; (NodoB < (NodoB < (NodoB < (HVaciaB) 2 (HVaciaB)) 3 (HVaciaB)) 6 (NodoB < (HVaciaB) 7 (HVaciaB)))))

;(test maxissorted 
;      (list
; (HVaciaB)
; (NodoB string<? (HVaciaB) "jdbc" (HVaciaB))
; (NodoB string<? (NodoB string<? (HVaciaB) "cdr" (HVaciaB)) "jdbc" (HVaciaB))
; (NodoB string<? (NodoB string<? (HVaciaB) "cdr" (NodoB string<? (HVaciaB) "foo" (HVaciaB))) "jdbc" (HVaciaB))))

;(apply beside (map printAB maxinsorted))
;(apply beside (map printAB maxissorted))