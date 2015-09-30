#lang plai

(require 2htdp/image)

;; any? Dado cualquier valor x, regresa #t
;
(define (any? x) #t)

;; ArbolB - Tipo de dato
;
(define-type  ArbolB
  [HVaciaB]
  [NodoB (c procedure?) ; Funcion de comparacion, recibe dos argumentos,
                        ; regresa un booleano.
         (l ArbolB?)
         (e any?) 
         (r ArbolB?)])

;; Abreviación de la hoja vacía
;
(define vc (HVaciaB))
;(test (HVaciaB) vc)

;; Abreviación del constructor de tipo NodoB para números y para strings
;
(define-syntax-rule (ndn l v r) (NodoB < l v r))
(define-syntax-rule (nds l v r) (NodoB string<? l v r))
;(test (ndn vc 4 vc) (NodoB < (HVaciaB) 4 (HVaciaB)))
;(test (nds vc "hola" vc) (NodoB string<? (HVaciaB) "hola" (HVaciaB)))

;; Ejemplos de árboles de números
;
(define arb1 (ndn vc 1 vc))
(define arb2 (ndn arb1 2 arb1))
(define arb3 (ndn arb2 3 arb2))
(define arb4 (ndn arb3 4 arb3))

(define arbN 
  (ndn 
   (ndn (ndn vc 4 vc) 2 (ndn vc 5 vc)) 
   1 
   (ndn (ndn (ndn vc 7 vc) 6 (ndn vc 9 vc)) 3 vc)))

(define maxiarb (ndn arbN 10 arbN))

;; Lista de árboles numéricos
;
(define arb-list (list arb1 arb2 arb3 arb4 arbN maxiarb))

;; Ejemplos de árboles de strings
;
(define arb1s (nds vc "a" vc))
(define arb2s (nds arb1s "b" arb1s))
(define arb3s (nds arb2s "c" arb2s))
(define arb4s (nds arb3s "d" arb3s))

(define arbNs 
  (nds 
   (nds (nds vc "adios" vc) "que" (nds vc "suc" vc)) 
   "cloj" 
   (nds (nds (nds vc "pow" vc) "ext" (nds vc "trial" vc)) "lambda" vc)))

(define maxiarbs (ndn arbNs "racket" arbNs))

;; Lista de árboles de strings
;
(define arbs-list (list arb1s arb2s arb3s arb4s arbNs maxiarbs))

;; Arbol base Wikipedia
;
(define arbol-base (nds (nds (nds vc "A" vc) "B" (nds (nds vc "C" vc) "D" (nds vc "E" vc))) 
                "F"
                (nds vc "G" (nds (nds vc "H" vc) "I" vc))))

;; Funciones auxiliares
;
(define (heightAB expArb)
  (type-case ArbolB
    expArb
    [HVaciaB () 0]
    [NodoB (c l e r) (max (+ 1 (heightAB l) ) (+ 1 (heightAB r)) )]))

(define (a-label v)
    (overlay
     (if (number? v)
         (text (number->string v) 12 "black")
         (text v 12 "black"))
     (circle 15 "solid" "lightblue")))

(define white-circle (circle 15 "solid" "white"))

(define nothing (circle 0 "solid" "white"))

(define (printAB-complete arb level)
  (type-case ArbolB arb
    [HVaciaB () (if (zero? level) nothing (above white-circle (beside (printAB-complete arb (sub1 level)) 
                                                                     (printAB-complete arb (sub1 level)))))]
    [NodoB (c l v r) (if (zero? level) 
                       (a-label v)
                       (let* ((lblack (line (* 15 (expt 2 (- level 2))) -30 "black"))
                              (rblack (line (* -15 (expt 2 (- level 2))) -30 "black"))
                              (lwhite (line (* 15 (expt 2 (- level 2))) -30 "white"))
                              (rwhite (line (* -15 (expt 2 (- level 2))) -30 "white"))
                              (d (above (a-label v) (beside (printAB-complete l (sub1 level))
                                                             (printAB-complete r (sub1 level))))))
                         (cond 
                           ((and (HVaciaB? l) (HVaciaB? r)) d)
                           ((HVaciaB? l) (underlay/align/offset "middle" "top" (beside lwhite rblack) 0 -15 d))
                           ((HVaciaB? r) (underlay/align/offset "middle" "top" (beside lblack rwhite) 0 -15 d))
                           (else (underlay/align/offset "middle" "top" (beside lblack rblack) 0 -15 d)))))]))

;; Imprime la representación gráfica de un ArbolB
;
(define (printAB arb)
  (printAB-complete arb (heightAB arb)))

;; Murales de árboles de números
;
(define arb-drawings (map printAB arb-list))
(define arb-mural (apply beside arb-drawings))

;; Murales de árboles de strings
;
(define arbs-drawings (map printAB arbs-list))
(define arbs-mural (apply beside arbs-drawings))