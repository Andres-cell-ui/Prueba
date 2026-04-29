#lang racket
;; Control Scheme

;; Implementacion de max-diff

;; Una forma, tambien implementar en Recursion de Cola
(define (max-diff L)
  (if (or (null? L) (null? (cdr L)))
      0
     (max (Diferencia L) (max-diff (cdr L)))))


(define (Diferencia L)
  (abs(- (car L) (cadr L))))



(define C1 (max-diff '(1 9 3 10 2)))



;; Pregunta 2 (b)

(define (procesar-datos fnSelector fnTrans fnComb lst)
  (apply fnComb (filter fnSelector (map fnTrans lst))))


;; Pregunta 2 (c)

(define en-rango (lambda(n)(lambda(m) (< n m))))

;; Pregunta 2 (d)

(define n 6)
(procesar-datos (en-rango n) (lambda (e) (+ e 1)) + (list 1 2 3 4 5 6))




;; Pregunta 2 Control 2023-2
;; DOM : LISTA X LISTA , Lo que recibes por parametros
;; REC : LISTA , Lo que retorna la funcion
;; Alternar elementos de una lista a otra
;; Recursion Natural (Opcional)
(define (alternar-elementos L1 L2)
  ;; Caso base
  (cond ((null? L1) L2)
        ((null? L2) L1)
        ;; Caso Recursivo
        (else
         (cons (car L1) (cons (car L2) (alternar-elementos (cdr L1) (cdr L2)))))))

;; Rercursion de Cola
;; DOM : LISTA X LISTA , Lo que recibes por parametros
;; REC : LISTA , Lo que retorna la funcion
;; Alternar elementos de una lista a otra
(define (alternar-elem-cola L1 L2)
  (define (alternar-aux L1 L2 acum)
    ;; Caso Base
    (cond ((null? L1) append(acum L2))
          ((null? L2) append(acum L1))

          ;; Caso Recursivo
          (else
           (alternar-aux (cdr L1) (cdr L2) (cons(car L1) (cons (car L2) acum))))))
    (alternar-aux L1 L2 '()))


;; Pregunta 1 Control 2023-2

(define fn (lambda (l n)
    (if (null? l)
        '()
        (if (= n 0)
            '()
            (cons (car l) (fn (cdr l) (- n 1))))))) ;; Dejo casos pendientes por cada nivel de recursion
;; Respuesta a

;; La función recorre la lista y construye una nueva lista con los primeros n elementos. El proceso se detiene cuando n llega a 0 o cuando la lista original se vacía

;; Respuesta b

;; Es una recursion Natural

;; Respuesta c
;; DOM : Lista X Numero Entero Positivo
;; REC : Lista
;; LO QUE HACE : Retorna una lista con los primeros n elementos de la lista l
;; Tipo de Recursion : de Cola
(define fn-cola (lambda (l n)
  (define (fn-aux l n acum)
    (if (null? l)
        (reverse acum)
        (if (= n 0)
            (reverse acum)
            (fn-aux (cdr l) (- n 1) (cons (car l) acum))))) 
  (fn-aux l n '())))