#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser. El
;; intérprete requiere un ambiente de evaluación en esta versión para buscar el valor de los 
;; identificadores.
;; interp: RCFBAEL/L Env -> RCFBAEL/L-Value
(define (interp expr env)
    (match expr
      [(num n) (numV n)]
      [(bool b) (boolV b)]
      [(op f param) (opf f (aux param env) env)]
      [(lisT p) (listV (aux p env))]
      [(id v)  (strict (lookup v env))]
      [(fun params body) (closureV params body env)]
      [(iF expr then-expr else-expr)
         (if (boolE? (interp expr env))
            (interp then-expr env)
            (interp else-expr env))]
      [(app fun-expr args) (let [(fun-val (strict (interp fun-expr env)))]
                            (interp
                             (closureV-body fun-val)
                              (makeEnv (closureV-params fun-val) args (closureV-env fun-val))))]))

(define (boolE? b)
  (equal? #t (boolV-b (strict b))))

(define (opf f list env)
  (match list
    ['() '()]
    [(cons x xs) (cond
                   [(or (equal? f +) (equal? f -) (equal? f *) (equal? f /) (equal? f min) (equal? f max) (equal? f mexpt) (equal? f modulo))
                    (numV (foldl f (numV-n x) (numt xs)))]
                   [(or (equal? f <) (equal? f >) (equal? f <=) (equal? f >=) (equal? f =) (equal? f diveq))
                    (boolV (foldl f (numV-n x) (numt xs)))]
                   [(or (equal? f pand) (equal? f por)) (boolV (foldl f (boolV-b x) (boolt xs)))]
                   [(or (equal? f car) (equal? f cdr)) (f (listV-elems x))]
                   [(equal? f empty?) (boolV (f (listV-elems x)))]
                   [(equal? f zero?) (bools (map f (numt list)))]
                   [else (map f (boolt list))])]))

(define (strict e)
   (match e
      [(exprV expr env) (strict (interp expr env))]
      [else e]))
      
(define (lookup name env)
   (match env
      [(mtSub) (error 'lookup "Free identifier")]
      [(aSub bound-name bound-value rest-env)
         (if (symbol=? bound-name name)
            bound-value
            (lookup name rest-env))]))

(define (makeEnv param val env)
  (match param
    ['() env]
    [(cons x xs) (makeEnv xs (cdr val) (aSub x (exprV (car val) env) env))]))

(define (aux lst env)
  (match lst
    ['() '()]
    [(cons x xs) (cons (strict (interp x env)) (aux xs env))]))

(define (numt list)
  (match list
    ['() '()]
    [(cons x xs) (cons (numV-n x) (numt xs))]))

(define (boolt list)
  (match list
    ['() '()]
    [(cons x xs) (cons (boolV-b x) (boolt xs))]))

(define (bools list)
  (match list
    ['() '()]
    [(cons x xs) (cons (boolV x) (bools xs))]))
