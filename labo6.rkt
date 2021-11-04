#lang racket
;Labo7


;exercice 1 Calcul du produit scalaire
( define V '(1 2 3))
( define T '(3 4 5))

( define (produitScalaire v t)

   (map (lambda (number1 number2)
          (* number1 number2))
       v t)
)

(produitScalaire V T)


;exercice 2 create matrice
( define ( matrice i j)
   (flat-map (lambda (k) (ligne k j)) (enumerer 1 i))

)

(define (enumerer a b)
  (if (> a b)
      '()
      (cons a (enumerer (+ a 1) b))))


(define (ligne k m)
  (map (lambda (x) (cons k x)) (enumerer 1 m))
)


(define (flat-map f L)
 (apply append (map f L)))
(matrice 2 4)

;Exercice 3 : Valeurs dans une matrice
( define (somme-matrice i j)
   (map (lambda (c)
          (list (car c)(cdr c) (+(car c)(cdr c))))
          (matrice i j))
                                           
)
(somme-matrice 1 3)

;Exercice 4 : Nombres premiers dans une matrice
( define (somme-matrice-premiers i j)

   (filter (lambda (c) (< (car c) (cadr c)))
           (somme-matrice i j))
)
(somme-matrice-premiers 1 3)
