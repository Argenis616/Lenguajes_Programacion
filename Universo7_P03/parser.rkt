#lang plai

(require "grammars.rkt")

;; Función que toma una expresión en sintaxis concreta y regresa el
;; árbol de sintaxis abstracta correspondiente.
;; parse : s-expression -> WAE
(define (parse sexp)
	(match sexp
          [(? symbol?) (id sexp)]
          [(? number?) (num sexp)]
          [(list 'with b body)
           (with (map bind b) (parse body))]
          [(list 'with* b body)
           (with* (map bind b) (parse body))]
          [(cons oper args) (op (elige oper) (map parse args))]))

;; Función auxiliar para parse.
;; bind : list -> Binding
(define (bind lst)
  (match lst
    [(list x xs) (binding x (parse xs))]))

;; Función que implementa la potencia multiparamétrica, la función
;; toma el primer elemento de la lista de parámetros y lo eleva al segundo,
;; luego eleva ese resultado al tercero y así sucesivamente.
;; mexpt : list -> Number
(define (mexpt lst)
  (if (= (length lst) 1) (first lst) (mexpt (cons (expt (first lst) (second lst)) (cdr (cdr lst))))))

;; [Auxiliar]. Función que hace un mapeo entre los operadores en
;; sintaxis concreta y los operadores de Racket. Esto con el fin de
;; aplicar la operación más adelante.
;; elige : symbol -> procedure
(define (elige sexp)
	(match sexp
          ['+ +]
          ['- -]
          ['* *]
          ['/ /]
          ['% modulo]
          ['min min]
          ['max max]
          ['pow mexpt]))
