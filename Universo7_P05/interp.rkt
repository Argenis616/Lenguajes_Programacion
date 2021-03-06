#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta sin azúcar sintáctica generado por
;; la función desugar. Esta versión de interp usa alcance estático.
;; interp: CFBAE/L -> CFBAE/L-Value
   (define (interp expr env)
    (match expr
      [(num n) (numV n)]
      [(bool b) (boolV b)]
      [(op f param) (opf f (aux param env) env)]
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
    [(cons x xs) (cons (interp x env) (aux xs env))]))

(define (numt list)
  (match list
    ['() '()]
    [(cons x xs) (cons (numV-n x) (numt xs))]))

(define (boolt list)
  (match list
    ['() '()]
    [(cons x xs) (cons (boolV-b x) (boolt xs))]))