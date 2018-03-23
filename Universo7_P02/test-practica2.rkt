#lang plai
;;Pruebas para funcion->string
(test(funcion->string (mul (cte 7) (x))) "(7 * x)")
(test(funcion->string (mul (cte 4) (x))) "(4 * x)")
(test(funcion->string (mul (cte 100) (x))) "(100 * x)")
(test(funcion->string (mul (cte 999) (x))) "(999 * x)")
(test(funcion->string (mul (cte 14) (x))) "(14 * x)")


;; Pruebas para evalua
(test(evalua (mul (cte 7) (x)) 200)(mul (cte 7) (cte 200)))
(test(evalua (mul (cte 27) (x)) 999)(mul (cte 27) (cte 999)))
(test(evalua (mul (cte 19) (x)) 1995)(mul (cte 19) (cte 1995)))
(test(evalua (mul (cte 666) (x)) 666)(mul (cte 666) (cte 666)))
(test(evalua (mul (cte 129) (x)) 1724)(mul (cte 129) (cte 1724)))

;; Pruebas para deriva.
(test(deriva (mul (cte 2) (x)))(sum (mul (cte 0) (x)) (mul (cte 1) (cte 2))))
(test(deriva (mul (cte 4) (x)))(sum (mul (cte 0) (x)) (mul (cte 1) (cte 4))))
(test(deriva (sum (cte 4) (x)))(sum (cte 0) (cte 1)))
(test(deriva (div (cte 4) (x)))(div (sum (mul (cte 0) (x)) (mul (mul (cte 1) (cte 4)) (cte -1))) (pot (x) 2)))

;;Pruebas para verifica.
 (test(verifica (afd '(p q r) '(a b) 'p d '(q)))#t)
 (test(verifica (afd '(p m r) '(a b) 'p d '(m e)))#f)
 (test(verifica (afd '(p m r) '(a b) 'p d '(m r p)))#t)
 (test(verifica (afd '(p m r e t y) '(a b) 'p d '(m r p y)))#t)
 (test(verifica (afd '(p m r e t y) '(a b) 'p d '(m r s y)))#f)
 
;;Pruebas para acepta.
 (test(acepta? (afd '(p q r) '(a b) 'p d '(q)) '(a a))#t)
 (test(acepta? (afd '(p q r m s) '(a b c) 'p d '(q)) '(a b))#f)
 (test(acepta? (afd '(p q r m s) '(a b c) 'p d '(q)) '(c c))#f)
 (test(acepta? (afd '(p q s) '(a b) 'p d '(q)) '(a a))#t)
 (test(acepta? (afd '(p q) '(a b) 'p d '(q)) '(a a))#t)
 
;;Pruebas para arrg.
 (define a (arrg number? 6 '(1 2 3 4 5)))
 (define b (arrg boolean? 5 '(1 2 3 4 5)))
 (test(arrg number? 5 '(1 2 3 4 5))(arrg number? 5 '(1 2 3 4 5)))
 (test(arrg boolean? 1 '(1))(arrg boolean? 1 '(1)))
 (test(calc-a a)"error: Dimensión inválida")
 (test(calc-a b)"error: Los elementos no son del tipo especificado")
 (test(arrg boolean? 2 '(1 3))(arrg boolean? 2 '(1 3)))

 ;;Prueas para obten-a.
 (test(calc-a (obten-a 2 a)) 3)
 (test(calc-a (obten-a -1 a))"error:indice invalido")
 (test(calc-a (obten-a -5 a))"error:indice invalido")
 (test(calc-a (obten-a 3 a)) 4)
 (test(calc-a (obten-a 4 a)) 5)
 

;;Pruebas para cjto de calc-c.
(test(calc-c (cjto '(1 1 7 2 9)))(cjto '(1 7 2 9)))
(test(calc-c (cjto '(4 7 22 4 8 )))(cjto '(4 7 22 8)))
(test(calc-c (cjto '(3 4 2 9)))(cjto '(3 4 2 9)))
(test(calc-c (cjto '(1 2)))(cjto '(1 2)))
(test(calc-c (cjto '()))(cjto '()))

;;Pruebas para esvacio? de calc-c.
(test(calc-c (esvacio? (cjto '(3 4 7 12)))) #f)
(test(calc-c (esvacio? (cjto '(6 33 2 7 8 12 3 0)))) #f)
(test(calc-c (esvacio? (cjto '(2 77 3 67 8 9 10)))) #f)
(test(calc-c (esvacio? (cjto '(7 6)))) #f)
(test(calc-c (esvacio? (cjto '()))) #t)

;;Pruebas para contiene? de calc-c.
(test(calc-c (contiene? (cjto '(2 3 4 7 1 99)) 8)) #f)
(test(calc-c (contiene? (cjto '(44 7 2 9 74 11 0 3)) 74)) #t)
(test(calc-c (contiene? (cjto '(24 3 6 87 12 43)) 6)) #t)
(test(calc-c (contiene? (cjto '(7 3 9 1 5 923 3)) 84)) #f)
(test(calc-c (contiene? (cjto '(2 3 5 1 8 76)) 16)) #f)

;;Pruebas para agrega-c de calc-c.
(test(calc-c (agrega-c (cjto'(3 45 2 12 9)) 4))(cjto '(3 45 2 12 9 4)))
(test(calc-c (agrega-c (cjto'(4 67 9 0 1)) 23))(cjto '(4 67 9 0 1 23)))
(test(calc-c (agrega-c (cjto'(3 3 3 56 3 2)) 1))(cjto '(3 56 2 1)))
(test(calc-c (agrega-c (cjto'(12 12 12 12 12)) 12))(cjto '(12)))
(test(calc-c (agrega-c (cjto'()) 4))(cjto '(4)))

;;Pruebas para union de calc-c.
(test(calc-c (union (cjto '(1 7 2 9))(cjto '(1 2 3 4)))) (cjto '(1 7 2 9 3 4)))
(test(calc-c (union (cjto '())(cjto '(1 2 3 4)))) (cjto '(1 2 3 4)))
(test(calc-c (union (cjto '())(cjto '()))) (cjto '()))
(test(calc-c (union (cjto '(1 2 3 4))(cjto '(1 2 3 4)))) (cjto '(1 2 3 4)))
(test(calc-c (union (cjto '(1 7 5 6))(cjto '(5 6 7 8 9)))) (cjto '(1 7 5 6 8 9)))
(test(calc-c (union (cjto '(1 23 45 3))(cjto '(1 2 3 4)))) (cjto '(1 23 45 3 2 4)))

;;Pruebas para interseccion de calc-c.
(test(calc-c (interseccion (cjto'(1 7 2 9)) (cjto '(1 2 3 4)))) (cjto '(1 2)))
(test(calc-c (interseccion (cjto'(23 4 7 3 1 22 9)) (cjto '(1 2 3 4)))) (cjto '(4 3 1)))
(test(calc-c (interseccion (cjto'(3 8 9 12 6)) (cjto '(1 7 4)))) (cjto '()))
(test(calc-c (interseccion (cjto'(2 3 9 1 8)) (cjto '(1 2 3 4)))) (cjto '(2 3 1)))
(test(calc-c (interseccion (cjto'(4 99 3 1 0)) (cjto '()))) (cjto '()))

;;Pruebas para diferencia de calc-c.
(test(calc-c (diferencia (cjto'(1 7 2 9)) (cjto '(1 2 3 4)))) (cjto '(7 9)))
(test(calc-c (diferencia (cjto'(24 25 99 0 1)) (cjto '(3 87 2)))) (cjto '(24 25 99 0 1)))
(test(calc-c (diferencia (cjto'(0 2 5 7)) (cjto '(2)))) (cjto '(0 5 7)))
(test(calc-c (diferencia (cjto'()) (cjto '(1 2 3 4)))) (cjto '()))
(test(calc-c (diferencia (cjto'()) (cjto '()))) (cjto '()))


