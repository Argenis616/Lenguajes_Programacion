#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 4 |#

;; Pruebas para  parse
;;Pruebas de parse para numeros.
(test(parse '1) (numS 1))
(test(parse '1729) (numS 1729))
(test(parse '77) (numS 77))
(test(parse '-12) (numS -12))
(test(parse '6.44) (numS 6.44))
(test(parse '1/4) (numS 1/4))

;;Pruebas de parse para id.
(test(parse 'foo) (idS 'foo))
(test(parse 'x) (idS 'x))
(test(parse 'y) (idS 'y))
(test(parse 'z) (idS 'z))
(test(parse 'hola) (idS 'hola))
(test(parse 'M) (idS 'M))

;;Pruebas de parse para booleanos.
(test(parse #t) (boolS #t))
(test(parse #f) (boolS #f))
(test(parse true) (boolS true))
(test(parse false) (boolS false))

;;Pruebas de parse para operadores.
(test(parse '(+ 1 2)) (opS + (list (numS 1) (numS 2))))
(test(parse '(+ 1 7 2 9)) (opS + (list (numS 1) (numS 7) (numS 2) (numS 9))))
(test(parse '(/ 2 7 8)) (opS / (list (numS 2) (numS 7)(numS 8))))
(test(parse '(* 3 0 4)) (opS * (list (numS 3) (numS 0)(numS 4))))
(test(parse '(- 8 2 1)) (opS - (list (numS 8) (numS 2)(numS 1))))
(test(parse '(min 3 4 4 4)) (opS min (list (numS 3) (numS 4) (numS 4)(numS 4))))

;;Pruebas de parse para fun.
(test(parse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})(appS
 (appS (funS '(x) (idS 'x)) (list (funS '(x) (opS + (list (idS 'x) (numS 5))))))
 (list (numS 3))))
(test(parse '{fun {x} {x}}) (funS '(x) (appS (idS 'x) '())))
(test(parse '{fun {x} {+ x 2}}) (funS '(x) (opS + (list (idS 'x) (numS 2)))))
(test(parse '{fun {x} {+ x y}})(funS '(x) (opS + (list (idS 'x) (idS 'y)))))
(test(parse '{fun {x} {- x y z}})(funS '(x) (opS - (list (idS 'x) (idS 'y) (idS 'z)))))

;;Pruebas de parse para app.
(test(parse '{app {fun {x} {x}} 1})(appS (idS 'app) (list (funS '(x) (appS (idS 'x) '())) (numS 1)))) 
(test(parse '{app {{{fun {x} x} {fun {x} {+ x 5}}} 3}})(appS
 (idS 'app)
 (list
  (appS
   (appS
    (funS '(x) (idS 'x))
    (list (funS '(x) (opS + (list (idS 'x) (numS 5))))))
   (list (numS 3))))))
(test(parse '{app {fun {x} {x}}})(appS (idS 'app) (list (funS '(x) (appS (idS 'x) '())))))
(test(parse '{app {fun {x} {+ x 2}}})(appS (idS 'app) (list (funS '(x) (opS + (list (idS 'x) (numS 2)))))))
(test(parse '{app {fun {x} {- x y z}}})(appS (idS 'app) (list (funS '(x) (opS - (list (idS 'x) (idS 'y) (idS 'z)))))))




;; Pruebas para  desugar

;;Pruebas de desugar para numeros.
(test(desugar(numS 1))(num 1))
(test(desugar(numS 1729))(num 1729))
(test(desugar(numS -12))(num -12)) 
(test(desugar(numS 6.44))(num 6.44)) 
(test(desugar(numS 1/4))(num 1/4)) 

;;Pruebas de desugar para id.
(test(desugar(idS 'x))(id 'x))
(test(desugar(idS 'foo))(id 'foo))
(test(desugar(idS 'hola))(id 'hola))
(test(desugar(idS 'y))(id 'y))

;;Pruebas de desugar para booleanos.
(test(desugar(boolS #t))(bool #t))
(test(desugar(boolS #f))(bool #f))
(test(desugar(boolS true))(bool true))
(test(desugar(boolS false))(bool false))

;;Pruebas de parse para operadores.
(test(desugar  (opS + (list (numS 1) (numS 2))))(op + (list (num 1) (num 2))))
(test(desugar (opS + (list (numS 1) (numS 7) (numS 2) (numS 9))))(op + (list (num 1) (num 7) (num 2) (num 9))))
(test(desugar  (opS / (list (numS 2) (numS 7)(numS 8))))(op / (list (num 2) (num 7) (num 8))))
(test(desugar (opS * (list (numS 3) (numS 0)(numS 4))))(op * (list (num 3) (num 0) (num 4))))
(test(desugar (opS * (list (numS 3) (numS 0)(numS 4))))(op * (list (num 3) (num 0) (num 4))))
(test(desugar (opS min (list (numS 3) (numS 4) (numS 4)(numS 4))))(op min (list (num 3) (num 4) (num 4) (num 4))))

;;Pruebas de desugar para fun.
(test(desugar(funS '(x) (appS (idS 'x) '())))(fun '(x) (app (id 'x) '())))
(test(desugar (funS '(x) (opS + (list (idS 'x) (numS 2)))))(fun '(x) (op + (list (id 'x) (num 2)))))
(test(desugar (funS '(x) (opS + (list (idS 'x) (idS 'y)))))(fun '(x) (op + (list (id 'x) (id 'y)))))
(test(desugar (funS '(x) (opS - (list (idS 'x) (idS 'y) (idS 'z)))))(fun '(x) (op - (list (id 'x) (id 'y) (id 'z)))))

;;Pruebas de desugar para app.
(test(desugar (appS (idS 'app) (list (funS '(x) (appS (idS 'x) '())))))
         (app (id 'app) (list (fun '(x) (app (id 'x) '())))))

(test(desugar(appS (idS 'app) (list (funS '(x) (opS + (list (idS 'x) (numS 2)))))))
        (app (id 'app) (list (fun '(x) (op + (list (id 'x) (num 2)))))))

(test(desugar(appS
 (idS 'app)
 (list
  (appS
   (appS
    (funS '(x) (idS 'x))
    (list (funS '(x) (opS + (list (idS 'x) (numS 5))))))
   (list (numS 3))))))
(app
 (id 'app)
 (list
  (app
   (app (fun '(x) (id 'x)) (list (fun '(x) (op + (list (id 'x) (num 5))))))
   (list (num 3))))))

;;Pruebas de desugar para numeros.
(test(desugar(numS 1))(num 1))
(test(desugar(numS 1729))(num 1729))
(test(desugar(numS -12))(num -12)) 
(test(desugar(numS 6.44))(num 6.44)) 
(test(desugar(numS 1/4))(num 1/4)) 

;;Pruebas de desugar para id.
(test(desugar(idS 'x))(id 'x))
(test(desugar(idS 'foo))(id 'foo))
(test(desugar(idS 'hola))(id 'hola))
(test(desugar(idS 'y))(id 'y))

;;Pruebas de desugar para booleanos.
(test(desugar(boolS #t))(bool #t))
(test(desugar(boolS #f))(bool #f))
(test(desugar(boolS true))(bool true))
(test(desugar(boolS false))(bool false))

;;Pruebas de parse para operadores.
(test(desugar  (opS + (list (numS 1) (numS 2))))(op + (list (num 1) (num 2))))
(test(desugar (opS + (list (numS 1) (numS 7) (numS 2) (numS 9))))(op + (list (num 1) (num 7) (num 2) (num 9))))
(test(desugar  (opS / (list (numS 2) (numS 7)(numS 8))))(op / (list (num 2) (num 7) (num 8))))
(test(desugar (opS * (list (numS 3) (numS 0)(numS 4))))(op * (list (num 3) (num 0) (num 4))))
(test(desugar (opS * (list (numS 3) (numS 0)(numS 4))))(op * (list (num 3) (num 0) (num 4))))
(test(desugar (opS min (list (numS 3) (numS 4) (numS 4)(numS 4))))(op min (list (num 3) (num 4) (num 4) (num 4))))

;;Pruebas de desugar para fun.
(test(desugar(funS '(x) (appS (idS 'x) '())))(fun '(x) (app (id 'x) '())))
(test(desugar (funS '(x) (opS + (list (idS 'x) (numS 2)))))(fun '(x) (op + (list (id 'x) (num 2)))))
(test(desugar (funS '(x) (opS + (list (idS 'x) (idS 'y)))))(fun '(x) (op + (list (id 'x) (id 'y)))))
(test(desugar (funS '(x) (opS - (list (idS 'x) (idS 'y) (idS 'z)))))(fun '(x) (op - (list (id 'x) (id 'y) (id 'z)))))

;;Pruebas de desugar para app.
(test(desugar (appS (idS 'app) (list (funS '(x) (appS (idS 'x) '())))))
         (app (id 'app) (list (fun '(x) (app (id 'x) '())))))

(test(desugar(appS (idS 'app) (list (funS '(x) (opS + (list (idS 'x) (numS 2)))))))
        (app (id 'app) (list (fun '(x) (op + (list (id 'x) (num 2)))))))

(test(desugar(appS
 (idS 'app)
 (list
  (appS
   (appS
    (funS '(x) (idS 'x))
    (list (funS '(x) (opS + (list (idS 'x) (numS 5))))))
   (list (numS 3))))))
(app
 (id 'app)
 (list
  (app
   (app (fun '(x) (id 'x)) (list (fun '(x) (op + (list (id 'x) (num 5))))))
   (list (num 3))))))



;; Pruebas para  interp

(test(interp (desugar (parse '1729)) (mtSub))(numV 1729))
(test(interp (desugar (parse '{+ 1 2 3 4 5})) (mtSub))(numV 15))
(test (interp (desugar (parse '{{fun {x} x} 3})) (mtSub)) (numV 3))
(test (interp (desugar (parse '{{{fun {x} x} {fun {x} {+ x 5}}} 3}))(mtSub)) (numV 8))
(test (interp (desugar (parse '3))(mtSub)) (numV 3))
(test(interp (desugar (parse '{with {{a 2} {b 3}} {+ a b}})) (mtSub))(numV 5))
(test(interp (desugar (parse '{with* {{a 5} {b {* a a a}}} b})) (mtSub))(numV 125))