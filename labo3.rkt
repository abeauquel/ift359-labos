#lang racket
;labo 3
(require racket/trace)
;exercice 1
(define (carre x)
  (* x x)
  
)

(define (produit a terme b)
  (if(> a b)
  1
  (* (terme a) (produit (+ a 1) terme b)))
)

;(produit 1 carre 3)


;exercice 2
(define (fact n)
  (produit 1 (lambda (a) (* a 1)) n)
)

;(fact 7)

;exercice 3
(define (accumule proc a terme b suiv elt)
   (if(> a b)
      elt
   (proc (terme a) (accumule proc (suiv a) terme b suiv elt))
   )

)

;(accumule + 1
;          (lambda (i) i)
;          4
;          (lambda(i) (+ 1 i))
;          0)

;exercice 4.1
;(accumule + 0
;          carre
;          3
;          (lambda(i) (+ 1 i))
;          0)

;exercice 4.2
;(accumule * 10
;          (lambda (i) i)
;          40
;          (lambda(i) (+ 10 i))
;          1)

;exercice 4.3
(define (carreS x)
  (cons x '^2)
  
)
;(accumule cons 1
;          carreS
;          10
;          (lambda(i) (+ 1 i))
;          null)

;exercice 5
(define (pi)
(accumule cons 1
          carreS
          10
          (lambda(i) (+ 1 i))
          null)
)


;exercice 6
;exercice 6.1
(define (produit-i a terme b)
  (define (produit2 compteur total)
    (if(> compteur b)
    total
    (produit2 (+ compteur 1) (* (terme compteur) total)))
    )
 (produit2 a 1)
)


;(produit 1 carre 3)
;(produit 10 carre 12)
;(produit-i 1 carre 3)
;(produit-i 10 carre 12)

;exercice 6.2
;(proc (terme a) (accumule proc (suiv a) terme b suiv elt))
 ;  )
(define (accumule-i proc a terme b suiv elt)
  (define (accumule2 compteur total)
   (if(> compteur b)
      total
      (accumule2 (suiv compteur ) (proc total (terme compteur)))
   ))

   (accumule2 a elt)
)


(accumule + 0
          carre
          3
          (lambda(i) (+ 1 i))
          0)
(accumule-i + 0
          carre
          3
          (lambda(i) (+ 1 i))
          0)


(accumule * 10
          (lambda (i) i)
          40
          (lambda(i) (+ 10 i))
          1)
(accumule-i * 10
          (lambda (i) i)
          40
          (lambda(i) (+ 10 i))
          1)
(accumule cons 1
          carreS
          10
          (lambda(i) (+ 1 i))
          null)
(accumule-i cons 1
          carreS
          10
          (lambda(i) (+ 1 i))
          null)

