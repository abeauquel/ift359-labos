#lang racket
;Labo8

(define-syntax cons-flot
(syntax-rules ()
((cons-flot a b) (cons a (delay b)))))

(define (car-flot f)
(car f))

(define (cdr-flot f)
(force (cdr f)))

(define (flot-null)
'())

(define (flot-null? flot)
(null? flot))




;Exercice 1 Calcul du produit scalaire
(displayln "Exerice 1 Calcul du produit scalaire")
(define (enumerer-int-flot> a)
  (cons-flot a (enumerer-int-flot> (+ a 1)))
)


;Exercice 2
(displayln "Exercice 2 ")
(define (flot->liste n flot)
   (if (= n 0)
       '()
       (cons (car-flot flot) (flot->liste (- n 1) (cdr-flot flot))))
   )


(displayln (flot->liste 10 (enumerer-int-flot> 10)))


;Exercice 3
(displayln "Exercice 3")
(define (map-flot proc flot)
  (cons-flot (proc (car-flot flot))
           (map-flot proc (cdr-flot flot)))
)

(define (mul-scal-flot a flot)
  (map-flot
            (lambda (x) (* x a))
            flot
  )
)
(displayln (flot->liste 10 (mul-scal-flot 5 (enumerer-int-flot> 10))))


;Exercice 4
(displayln "Exercice 4")
(define (add-flot flot1 flot2)
   (cons-flot (+ (car-flot flot1) (car-flot flot2))
           (add-flot (cdr-flot flot1) (cdr-flot flot2)
                     ))
)

(displayln (flot->liste 10 (add-flot (enumerer-int-flot> 1) (enumerer-int-flot> 10))))


;Exercice 5
(displayln "Exercice 5")
(define (flot-infini-de i)
  (cons-flot i (flot-infini-de i))
 )

(displayln (flot->liste 10 (flot-infini-de 1)))

;Exercice 6
(displayln "Exercice 6")
(define (entiers)
  (add-flot (flot-infini-de 0) (enumerer-int-flot> 0))
  )

(displayln (flot->liste 100 (entiers)))

;Exercice 7
(displayln "Exercice 7")
(define F-magic1
 (cons-flot 1 (mul-scal-flot 2 F-magic1))) ;1 2*1 2*2 4*2 8*2
(define F-magic2
 (cons-flot 1 (add-flot F-magic2 F-magic2))) ; 1 1+1 2+2 4+4 8+8

(displayln (flot->liste 10 F-magic1))
(displayln (flot->liste 10 F-magic2))

;Exercice 8
(displayln "Exercice 8")
;0, 1, 1, 2, 3, 5, 8, 13, 21, 34,

(define (F-magic3 un deux)
 (cons-flot un (add-flot un deux)))

(define fibonacci
  (cons-flot 0 (cons-flot 1 (F-magic3 1 1) ))
)

(displayln (flot->liste 10 fibonacci))
