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
           (map-flot (cdr-flot flot))
)

(define (mul-scal-flot a flot)
  (map-flot
            (lambda (x) (* x a))
            flot
  )
)
(displayln (flot->liste 10 (mul-scal-flot 5 (enumerer-int-flot> 10))))

;flot infini du signalsignal de la sinusoïde déterminée
;par la fonction: F(x) = a*sin(b* t + c)
(define (gen-signal a b dt c)
 (cons-flot (* a (sin (* b (+ dt c))))
            (gen-signal a b dt (+ c 1))
            )
)

;test simple flot
(define (flot-infini-de i)
  (cons-flot i (flot-infini-de i))
 )


