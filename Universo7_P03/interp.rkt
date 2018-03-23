#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser.
;; interp: WAE -> number
(define (interp exp)
     (match exp
       [(num n) n]
       [(id i) error 'interp "Identificador libre"]
       [(op p body) (let [(int-body (map interp body))]
                      (if (equal? p mexpt) (mexpt (map interp body))
                      (foldr p (car int-body) (cdr int-body))))]
       [(with binds body) (interp (subst-lst binds body))]
       [else error "Error"]))

;; Función que implementa el algoritmo de sustitución.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sub-id val)
	(match expr
          [(id i) (if (symbol=? i sub-id) val expr)]
          [(num n) expr]
          [(op f args) (op f (aux args sub-id val))]
          [(with bindings body)
                (if (contain bindings sub-id)
                    (with (subst-with bindings sub-id val) body)
                    (with (subst-with bindings sub-id val) (subst body sub-id val)))]
          [(with* bindings body)
                (if (contain bindings sub-id)
                    (with (subst-with bindings sub-id val) body)
                    (with (subst-with bindings sub-id val) (subst body sub-id val)))]
          ))

(define (subst-with bindings sub-id value)
  (match bindings
    ['() '()]
    [(cons (binding a b) xs) (cons (binding a (subst b sub-id value)) (subst-with xs sub-id value))]))

;; Función auxiliar para subst.
;; aux: list symbol WAE -> list
(define (aux lst sub-id val)
     (match lst
       ['() '()]
       [(cons x xs) (cons (subst x sub-id val) (aux xs sub-id val))]))

;; Función auxiliar para ver si un sub-id está contenido en
;; una lista de bindings.
;; contain: list symbol -> boolean
(define (contain lst sub-id)
  (match lst
    ['() false]
    [(cons (binding a b) xs) (if (symbol=? a sub-id) true (contain xs sub-id))]))

;; Función que substituye una lista de bindings a una expresión.
;; subst-lst: list WAE -> WAE.
(define (subst-lst bindings body)
  (match bindings
    ['() body]
    [(cons (binding var val) xs) (let [(res (subst body var val))] (subst-lst xs res))]))
