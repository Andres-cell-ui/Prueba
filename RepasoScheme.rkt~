#lang racket 

;; General
(define (usuario nombre canciones reproducciones)
  (list nombre canciones reproducciones))

(define (reproducciones nombre lista-canciones num-max tiempo )
  (list nombre lista-canciones num-max tiempo ))

;; Selector
(define (get-nombre U)
  (list-ref U 0))

;; Modificador
(define (set-canciones newC U)

  
  (usuario
   (list-ref U 0)
   (cons newC (list-ref U 1))
   (list-ref U 2)))



(define f (lambda(x) (* x 2)))
(define g (lambda(x) (+ x 3)))
(define h (compose f g))




(define lista '(10 20 30 40 50))



;; Recursion Cola
(define (elemN L n)
  ;;Caso Base
  (if (= n 0)
      (car L)
      ;;Caso Recursivo
      (elemN (cdr L) (- n 1))))

;;Recursion Natural
(define (SumLista L)
  ;;Caso base
  (if (null? L) 0
      (+ (car L)(SumLista (cdr L)))))

