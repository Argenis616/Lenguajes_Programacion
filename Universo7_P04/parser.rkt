#lang plai

(require "grammars.rkt")

;; Analizador sintáctico para WAE.
;; Dada una s-expression, construye el árbol de sintaxis abstracta correspondiente.
;; parse: s-expression -> FWBAE
(define (parse sexp)
   (cond
      [(boolean? sexp) (boolS sexp)] ; para booleans
      [(symbol? sexp) (idS sexp)] ; para identificadores
      [(number? sexp) (numS sexp)] ; para números
      [(list? sexp)
         (case (car sexp)
            [(+ - * - / % max min pow < > <= >= /= = not and or) ; para operaciones
               (opS
                  (elige (car sexp)) (map parse (cdr sexp)))]
            [(with) ; para asignaciones locales
               (withS
                  (map bind (first (rest sexp)))
                  (parse (first (rest (rest sexp)))))]
           [(with*) ; para asignaciones locales
               (withS*
                  (map bind (first (rest sexp)))
                  (parse (first (rest (rest sexp)))))]
            [(fun) ; para lambdas
               (funS
                  (first (rest sexp))
                  (parse (first (rest (rest sexp)))))]
            [else ; para aplicación de funciones
               (appS
                  (parse (first sexp))
                  (map parse (rest sexp)))])]))

;; Función auxiliar para parse.
;; bind : list -> Binding
(define (bind lst)
  (match lst
    [(list x xs) (binding x (parse xs))]))

;; Función auxiliar para obtener los nombres de los bindings.
;; getName : list -> list
(define (getName lst)
  (match lst
    ['() '()]
    [(cons (binding name value) xs) (cons name (getName xs))]))

;; Función auxiliar para obtener las expresiones de los bindings.
;; getName : list -> list
(define (getExpr lst)
  (match lst
    ['() '()]
    [(cons (binding name value) xs) (cons (desugar value) (getExpr xs))]))

;; Función que implementa la potencia multiparamétrica, la función
;; toma el primer elemento de la lista de parámetros y lo eleva al segundo,
;; luego eleva ese resultado al tercero y así sucesivamente.
;; mexpt : list -> Number
(define (mexpt l r)
  (expt l r))

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
          ['pow mexpt]
          ['< <]
          ['> >]
          ['<= <=]
          ['>= >=]
          ['= =]
          ['/= diveq]
          ['not not]
          ['or por]
          ['and pand]))

;; Implementación de la operación /=.
(define (diveq l r)
  (not (equal? l r)))

(define (por l r)
  (or l r))

(define (pand l r)
  (and l r))

;; Función auxiliar para convertir withS* en with.
(define (convert sexp)
  (match sexp
    [(withS* '() body) body]
    [(withS* (cons x xs) body) (withS (list x) (convert (withS* xs body)))]))

;; Función que elimina el azúcar sintáctica de las expresiones de FWBAE, es decir las convierte a 
;; expresiones de FBAE.
;; desugar: FWBAE -> FBAE
(define (desugar expr)
  (match expr
      ; Un número sigue siendo un número
      [(numS n) (num n)]
      ; Un booleano sigue siendo un booleano
      [(boolS b) (bool b)]
      ; Convertimos a la operación binaria quitando el azúcar sintactica a su
      ; lista de argumentos.
      [(opS f args) (op f (map desugar args))]
      ; A with le quitamos el azúcar sintáctica convirtiéndolo en una aplicación
      ; de función, donde la función recibe como parámetro el identificador,
      ; el cuerpo no se modifica y realizamos la aplicación con el valor del
      ; identificador.
      [(withS binds body) 
         (app (fun (getName binds) (desugar body)) (getExpr binds))]
      ; A with* le quitamos el azúcar sintáctica convirtiéndolo en withs anidados
      ; que a su vez tienen azúcar sintáctica.
      [(withS* binds body)
         (desugar (convert expr))]
      ; Un identificador sigue siendo un identificador
      [(idS name) (id name)]
      ; Convertimos las funciones quitando el azúcar sintáctica a su cuerpo
      [(funS param body) (fun param (desugar body))]
      ; Convertimos las aplicaciones quitando el azúcar sintáctica a la función
      ; y al argumento recursivamente.
      [(appS fun-expr arg-expr) (app (desugar fun-expr) (map desugar arg-expr))]))