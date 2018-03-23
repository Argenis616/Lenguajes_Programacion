#lang plai


;; Función tribonacci que recibe como parametro un numero y nos regresa su valor en
;; la sucesion de tribonacci utilizando recursion
;; tribonacci: number -> number
(define (tribonacci n)
   (cond
    [(<= n 1) 0]
    [(= n 2) 1]
    [else
      (+ (tribonacci (- n 1)) (tribonacci (- n 2)) (tribonacci (- n 3)))]))

;; Función tribonacci que recibe como parametro un numero y nos regresa su valor en
;; la sucesion de tribonacci utilizando recursion de cola.
;; tribonacciC: number -> number
(define (tribonacciC n)
  (tribonacci-cola n 0 0 1))

;; Funcion auxiliar que se encarga de aplicar recursion de cola tomando como parametros
;; el numero deseado y tres acumuladores que en un inicio tienen el valor de los casos base.
;; tribonacciC: number number number -> number
(define (tribonacci-cola n acc1 acc2 acc3)
  (cond
    [(<= n 0) 0]
    [(= n 2) acc3]
    [else
       (tribonacci-cola (- n 1) acc2 acc3 (+ acc1 acc2 acc3))]))



;;Funcion que nos ayuda a definir la tabla de dispercion para la funcion de 
;;tribonacci-memo.
(define tabla (make-hash (list (cons 0 0)
                               (cons 1 0)
                               (cons 2 1))))


;; Funcion que toma como parametro un numero y regresa su valor en la sucesion de 
;; tribonacci utilizando la tecnica de memorizacion.
;; tribonacciC: number -> number
(define (tribonacci-memo n)
  (let ([res (hash-ref tabla n "ninguno")])
    (cond
      [(equal? res "ninguno")
       (hash-set! tabla n (+ (tribonacci-memo (- n 1)) (tribonacci-memo (- n 2)) 
                             (tribonacci-memo (- n 3))))
       (hash-ref tabla n)]
      [else res])))
  


