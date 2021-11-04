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