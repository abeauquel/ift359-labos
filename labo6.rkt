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

(display "Exercice 5 permutation\n")
;Exercice 5 permutation
(define (permutation L)
  (if (null? L)
      (list null)
      (flat-map (lambda (x)
                  (map (lambda(p) (cons x p));rajouter x à 
                       (permutations (oter x L)))); toutes les permutations sans l'élément
                L)))

(define (oter x L)
   (filter (lambda (a) (not(= a x))) L)
)

(permutations '(1 2 3 ))
(display "Exercice 6 Arbre n-aire\n")

;Exercice 6 Arbre n-aire
(define (creer-arbre-n etiq L-fils)
  
)

(define (creer-arbre-vide-n)
  '()
)