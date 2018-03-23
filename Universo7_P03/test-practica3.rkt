#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 3 |#

;; Pruebas para  parse

;;Parte 1 parse
(test(parse 'x)(id 'x))
(test(parse 'y)(id 'y))

;;Parte 2 parse
(test(parse '5)(num 5))
(test(parse '-2)(num -2))
(test(parse '1/2)(num 1/2))
(test(parse '1.23454)(num 1.23454))
(test(parse '2+3i)(num 2+3i))

;;Parte 3 parse
(test(parse '(+ 1 2 7))(op + (list (num 1) (num 2) (num 7))))
(test(parse '(* 4 2 3))(op * (list (num 4) (num 2) (num 3))))
(test(parse '(- 1 2 3))(op - (list (num 1) (num 2) (num 3))))
(test(parse '(/ 8 4 2))(op / (list (num 8) (num 4) (num 2))))
(test(parse '(% 4 2 2))(op modulo (list (num 4) (num 2) (num 2))))
(test(parse '(min 1 2 3))(op min (list (num 1) (num 2) (num 3))))
(test(parse '(max 1 7 2))(op max (list (num 1) (num 7) (num 2))))
(test(parse '(pow 2 3 3))(op mexpt (list (num 2) (num 3) (num 3))))
(test(parse '(pow 3 3 3))(op mexpt (list (num 3) (num 3) (num 3))))
(test(parse '(+ 3 4))(op + (list (num 3) (num 4))))
(test (parse '{* 3 6.5 9.7}) (op * (list (num 3) (num 6.5) (num 9.7))))

;;parte 4 parse
(test(parse '{with {{x 5}} {+ 3 3}})(with (list (binding 'x (num 5))) (op + (list (num 3) (num 3)))))
(test(parse '{with {{x 5}} {with {{y 7}} y}})(with (list (binding 'x (num 5))) (with (list (binding 'y (num 7))) (id 'y))))
(test(parse '{with {{x {with {{y 5}} y}}} {with {{z 2}} {with{{m 3}} {with{{n 2}} {+ z z}}}}})(with
 (list (binding 'x (with (list (binding 'y (num 5))) (id 'y))))
 (with
  (list (binding 'z (num 2)))
  (with
   (list (binding 'm (num 3)))
   (with (list (binding 'n (num 2))) (op + (list (id 'z) (id 'z))))))))

;;Parte 5 parse
(test(parse '{with* {{x 5} {y 4} {z x}} {+ x x}})(with*
 (list (binding 'x (num 5)) (binding 'y (num 4)) (binding 'z (id 'x)))
 (op + (list (id 'x) (id 'x)))))
(test(parse '{with {{x 5} {y 3} {z x}} {with {{y x}} y}})(with
 (list (binding 'x (num 5)) (binding 'y (num 3)) (binding 'z (id 'x)))
 (with (list (binding 'y (id 'x))) (id 'y))))
(test(parse '{with* {{x {with {{y x}} y}} {l 3}} {with {{z 2}} {with{{m 3}} {with{{n 2}} {+ z z}}}}})(with*
 (list (binding 'x (with (list (binding 'y (id 'x))) (id 'y))) (binding 'l (num 3)))
 (with
  (list (binding 'z (num 2)))
  (with
   (list (binding 'm (num 3)))
   (with (list (binding 'n (num 2))) (op + (list (id 'z) (id 'z))))))))

;; Pruebas para  interp

;;Parte 1 interp
(test(interp(parse 'x))"Identificador libre")
(test(interp(parse 'y))"Identificador libre")

;;Parte 2 interp.
(test (interp (parse '5)) 5)
(test(interp(parse '-2)) -2)
(test(interp(parse '1/2)) 1/2)
(test(interp(parse '1.23454)) 1.23454)
(test(interp(parse '2+3i)) 2+3i)

;;Parte 3 parse
(test(interp(parse '(+ 1 2 7))) 10)
(test(interp(parse '(* 4 2 3))) 24)
(test(interp(parse '(- 7 2 3))) 6)
(test(interp(parse '(/ 8 4 2))) 16)
(test(interp(parse '(% 4 2 2))) 0)
(test(interp(parse '(min 1 2 3))) 1)
(test(interp(parse '(max 1 7 2 9))) 9)
(test(interp(parse '(pow 2 3))) 8)
(test(interp(parse '(pow 3 3))) 27)
(test(interp(parse '(+ 3 4 8))) 15)

;;parte 4 interp
(test(interp(parse '{with {{x 5}} {+ 3 3}})) 6)
(test(interp(parse '{with {{x 5}} {with {{y 7}} y}})) 7)
(test(interp(parse '{with {{x {with {{y 5}} y}}} {with {{z 2}} {with{{m 3}} {with{{n 2}} {+ z z}}}}})) 4)



