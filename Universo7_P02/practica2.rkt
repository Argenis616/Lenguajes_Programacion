#lang plai


#| Práctica 2: Tipos de datos abstractos |#

;; TDA para representar funciones.
;; Se tienen constructores para la variable independiente, constantes, suma de funciones, 
;; multiplicación de funciones, división de funciones y potencia.
(define-type funcion
  [x]
  [cte (n integer?)]
  [sum (f funcion?) (g funcion?)]
  [mul (f funcion?) (g funcion?)]
  [div (f funcion?) (g funcion?)]
  [pot (f funcion?) (n integer?)])

;; Función que regresa una cadena de caracteres, representando a la función que recibe como parámetro.
;; funcion->string: funcion -> string
(define (funcion->string f)
   (match f
     [(x) "x"]
     [(cte n) (number->string n)]
     [(sum f g) (string-append "(" (funcion->string f) " + " (funcion->string g) ")")]
     [(mul f g) (string-append "(" (funcion->string f) " * " (funcion->string g) ")")]
     [(div f g) (string-append "(" (funcion->string f) " / " (funcion->string g) ")")]
     [(pot f n) (string-append (funcion->string f) "^"  (number->string n))]))

;; Función que devuelve el resultado de evaluar la función pasada como parámetro con el valor v, es
;; decir, regresa f(v).
(define (evalua f v)
   (match f
     [(x) (cte v)]
     [(cte n) (cte n)]
     [(sum f g) (sum (evalua f v) (evalua g v))]
     [(mul f g) (mul (evalua f v) (evalua g v))]
     [(div f g) (div (evalua f v) (evalua g v))]
     [(pot f n) (pot (evalua f v) n)]))

;; Función que regresa la derivada de una función.
;; deriva: funcion -> funcion
(define (deriva f)
   (match f
     [(x) (cte 1)]
     [(cte n) (cte 0)]
     [(sum f g) (sum (deriva f) (deriva g))]
     [(mul f g) (sum (mul (deriva f) g) (mul (deriva g) f))]
     [(div f g) (div (sum (mul (deriva f) g) (mul (mul (deriva g) f) (cte -1))) (pot g 2))]
     [(pot f n) (mul (cte n) (pot f (- n 1)))]))

; ------------------------------------------------------------------------------------------------ ;

;; TDA para representar autómatas finitos deterministas.
;; Se tiene un único constructor para representar autómatas finitos deterministas como la quintúpla
;; correspondiente.
(define-type automata
   [afd (estados list?) (alfabeto list?) (inicial symbol?) (transicion procedure?) (finales list?)])

;; Función que verifica que un afd está bien construido. Verifica que el símbolo inicial y el conjunto
;; de símbolos finales pertenecen al conjunto de estados.
;; verifica: automata -> boolean
(define (verifica atm)
   (match atm
     [(afd e a i t f) (and (member i e) (contiene f e))]))

;; Función de transición .
;; d : symbol symbol - > symbol
( define ( d estado simbolo )
( match estado
   ['a 'q]
[ 'p (if ( symbol=? 'a simbolo ) 'q 'r )]
[ 'q (if ( symbol=? 'a simbolo ) 'q 'r )]
[ 'r 'r ]))

;; Función auxiliar que checa si la lista 1 está contenida en la lista 2.
;; contiene: list -> list -> boolean
    (define (contiene l1 l2)
        (cond
            [(null? l1) true]
            [(null? l2) false]
            [(member (car l1) l2) (contiene (cdr l1) l2)]
            [else false]))

;; Predicado que dado un afd y una lista de símbolos que representan una expresión, deterimina si es
;; aceptada por el autómata.
(define (acepta? atm lst)
   (match atm
     [(afd e a i t f) (if (member (aux t i lst) f) true false)]))

;; Funcion auxiliar para la funcion acepta?.
(define (aux fun estado lst)
  (match lst
    ['() estado]
    [(cons x xs) (aux fun (fun estado x) xs)]))

; ------------------------------------------------------------------------------------------------ ;

;; TDA para representar una gramática de arreglos.
;; Se tienen constructores que permiten definir un arreglo, para especificar la operación de agregar 
;; un elemento y un tercero para obtener elemento.
(define-type arreglo
  [arrg (tipo procedure?) (dim number?) (elems list?)]
   ;;[agrega-a (e integer?) (a arreglo?) (i integer?)]
   [obten-a (i number?) (a arreglo?)])

;; Función que evalúa expresiones de tipo arreglo.
;; calc-a: arreglo -> arreglo
(define (calc-a arr)
  (match arr
     [(arrg tipo dim elems)
      (if(equal? dim (length elems))
         (if (ormap tipo elems)
             (arrg tipo dim elems)
         "error: Los elementos no son del tipo especificado")
      "error: Dimensión inválida")]
     ;;[(agrega-a e a i)
     ;;(list-set  (arrg-elems a) 2 5)]
     [(obten-a i a) 
      (if(positive? i)
         (list-ref (arrg-elems a) i )
      "error:indice invalido")]))
; ------------------------------------------------------------------------------------------------ ;

;; TDA para representar una gramática de conjuntos.
;; Se tienen constructores que permiten definir un conjunto, determinar si el conjunto es vacío, 
;; determinar si un elemento está contenido en el conjunto, agregar un elemento, unir conjunto, 
;; intersectar conjunto y calcular la diferencia.
(define-type conjunto
   [cjto  (l list?)]
   [esvacio? (c conjunto?)]
   [contiene? (c conjunto?) (e number?)]
   [agrega-c (c conjunto?) (e number?)]
   [union (c1 conjunto?) (c2 conjunto?)]
   [interseccion (c1 conjunto?) (c2 conjunto?)]
   [diferencia (c1 conjunto?) (c2 conjunto?)])
  
;; Función que evalúa expresiones de tipo conjunto.
;; calc-c: conjunto -> conjunto
(define (calc-c cto)
  (match cto
     [(cjto l) (cjto (remove-duplicates l))]
     [(esvacio? c)
      (if(equal? (length(cjto-l c)) 0)#t #f)]
    [(contiene? c e) 
      (if(equal? (memq e (cjto-l c)) #f) #f #t)]
    [(agrega-c c e) 
     (cjto(remove-duplicates(append (cjto-l c) (list e ))))]
    [(union c1 c2)
     (cjto(remove-duplicates(append (cjto-l c1) (cjto-l c2))))]
    [(interseccion c1 c2)
     (cjto(interseccion2(cjto-l c1) (cjto-l c2)))]
    [(diferencia c1 c2)
     (cjto(diferencia2(cjto-l c1) (cjto-l c2)))]))
    
;; Función que revisa si un elemento esta dentro de una lista.
;; contiene-aux: lista  elemento -> booleano
(define(contiene-aux? c e)
     (cond[(equal? (memq e c) #f) #f]))

;; Función que calcula la interseccion de dos listas.
;; intersecion: lista  lista -> lista.
(define (interseccion2 c1 c2)
(if (null? c1) null
(if (null? c2) null
(if (contiene-aux? c2 (car c1)) 
    (cons (car c1) (interseccion2 (cdr c1) c2))
(interseccion2 (cdr c1) c2)))))

;; Función que calcula la diferencia de dos listas.
;; diferencia2: lista  lista -> lista
(define (diferencia2 c1 c2)
(if (null? c1) null
(if (null? c2) c1
(if (contiene-aux? (interseccion2 c1 c2) (car c1))
    (diferencia2 (cdr c1) c2)
(cons (car c1) (diferencia2 (cdr c1) c2))))))

