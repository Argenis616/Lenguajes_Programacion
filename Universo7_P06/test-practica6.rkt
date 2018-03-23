#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 5|#

;;Pruebas para parse

(test(parse '{if {> 2 3 4 5 6} 9 6}) (ifS
 (opS > (list (numS 2) (numS 3) (numS 4) (numS 5) (numS 6)))
 (numS 9)
 (numS 6)))



(test(parse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})(appS
 (appS (funS '(x) (idS 'x)) (list (funS '(x) (opS + (list (idS 'x) (numS 5))))))
 (list (numS 3))))
 
(test(parse '{cond {{ < 2 3} 1}{{ > 10 2} 2}{{ < 3 4} 4}{{ > 6 4} 9}{{ > 1 2} 4}{ else 3}})
 (condS
 (list
  (condition (opS < (list (numS 2) (numS 3))) (numS 1))
  (condition (opS > (list (numS 10) (numS 2))) (numS 2))
  (condition (opS < (list (numS 3) (numS 4))) (numS 4))
  (condition (opS > (list (numS 6) (numS 4))) (numS 9))
  (condition (opS > (list (numS 1) (numS 2))) (numS 4))
  (else-cond (numS 3)))))

;Pruebas para el desugar

(test(desugar(parse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z}))(app
 (fun'(x)
  (app
   (fun '(y) (app (fun '(z) (id 'z)) (list (op + (list (id 'x) (id 'y))))))
   (list (op + (list (num 2) (id 'x))))))
 (list (num 3))))

(test (desugar (parse '{app {fun {x} {x}} 1})) 
      (app (id 'app) (list (fun '(x) (app (id 'x) '())) (num 1))))

(test (desugar (parse '{fun {x} {x}})) (fun '(x) (app (id 'x) '())))

(test(desugar(parse '{cond {true 1} {true 2} {else 3}}))(num 1))

(test(desugar(parse '{+ {- 3 4} 7}))(op + (list (op - (list (num 3) (num 4))) (num 7))))

;;Pruebas para el interp
(test(interp (desugar (parse '{if #t #f #f})) (mtSub))(boolV #f))

(test (interp (desugar (parse 'x)) (aSub 'x (numV 2) (mtSub))) (numV 2))

(test(interp (desugar (parse '1729)) (mtSub))(numV 1729))

(test (interp (desugar (parse '{fun {x} {x}})) (mtSub) ) (closureV '(x) (app (id 'x) '()) (mtSub)))


