#lang racket
;Labo5


;exercice 1 labo1
(define (carre x)
  (* x x))
;exercice 3 labo1
(define (accumule proc a terme b suiv elt)
   (if(> a b)
      elt
   (proc (terme a) (accumule proc (suiv a) terme b suiv elt))
   )
)

;ex 1
( define ( f1 x y)
   (let ([a (+ 1 (* x y))]
         [b (- 1 y)])
     (+ (* x (* a a))
        (* y b)
        (* a b)))
)

( define ( f2 x y)
  ((lambda (a b)
    (+ (* x (* a a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y))
)

;ex 2 fonction de lissage
( define (lissage f dx)

   (lambda (x)

     (/
      (+ (f (- x dx)) (f x) (f (+ x dx)) )
      3
      )
     )
)

;(accumule cons 1
;          (lambda (i) i)
;          10
;          (lambda(i) (+ 1 i))
;          `())
;
;(accumule cons 1
;          (lambda (i) ((lissage carre 0.01)i))
;          10
;           (lambda(i) (+ 1 i))
;          `())

(define (variable? exp)
  (symbol? exp)
)

(define (somme? exp)
  (and (list? exp) (equal? (car exp) '+ ))
)

(define (operande1 exp)
  (if (list? exp)
      (cadr exp)
      ("erreur")
      ))

(define (operande2 exp)
  (if (list? exp)
      (caddr exp)
      ("erreur")
      ))

(define (faire-somme op1 op2)
  (list '+ op1 op2)
  )

(define (deriv exp x)
   (cond
     [(number? exp) 0]
     [(variable? exp) (if(equal? exp x) 1 0)]
     [(somme? exp) (faire-somme (deriv (operande1 exp) x) (deriv (operande2 exp) x))]
     [else "expression inconnue"]
         ))

(deriv '(+ 1 2) 'x)
(deriv '(+ 1 x) 'x)
 