#lang racket
;;Pep 2/2025
;; EJERCICIO 1
;; TDA Agenda y Contacto
;; Constructor Contacto
(define (contacto nombre apellido numero)
  (list nombre apellido (cons numero '())))
;; Constructor Agenda
(define (agenda)'())


;; Selector Contacto
(define (get-nombre contacto)
  (car contacto))
(define (get-apellido contacto)
  (list-ref contacto 1))

(define (get-numero contacto)
  (car (list-ref contacto 2)))

(define (get-lista-numeros contacto)
  (list-ref contacto 2))

;; Pregunta A
(define (count agenda)
  (define (count-aux agenda acum)
    (if (null? agenda)
        acum
        (count-aux (cdr agenda) (+ acum 1))))
  (count-aux agenda 0))

;; Pregunta B
(define (count-lazy-aux agenda fn)
  (if (null? agenda)
      0
      (if (fn (car agenda))
          (+ 1  (count-lazy-aux (cdr agenda) fn))
          (count-lazy-aux (cdr agenda) fn))))

(define (count-lazy agenda fn)
  (delay(count-lazy-aux agenda fn)))

;; Pregunta C
(define find (lambda(agenda)
              (lambda(nombre)
                (lambda(apellido)
                  (lambda(numero)
                    (filter(lambda(c)
                             (or (string-contains? (get-nombre c) nombre)
                                 (equal? nombre (get-nombre c))
                             
                                 (string-contains? (get-apellido c) apellido)
                                 (equal? apellido (get-apellido c))
                                 
                                 (string-contains? (get-numero c) numero)
                                 (equal? numero (get-numero c))))
                           agenda))))))

;; Pregunta D

;; Primero agregamos un numero sin repetidos

(define (agregar-num-sin-repetidos num lista-numeros)
  (if(null? lista-numeros)
     (list num)
     (if (equal? num (car lista-numeros))
         lista-numeros
         (cons (car lista-numeros) (agregar-num-sin-repetidos num (cdr lista-numeros))))))

;; Creamos la nueva agenda
(define (insertar-contacto c agenda)
  (if (null? agenda)
      (cons c agenda)
      (if (and (equal? (get-nombre c) (get-nombre (car agenda)))
               (equal? (get-apellido c) (get-apellido (car agenda))))
          (cons (list (get-nombre c) 
                      (get-apellido c) 
                      (agregar-num-sin-repetidos (get-numero c) (get-lista-numeros (car agenda))))
                (cdr agenda))
          
          
          (cons (car agenda) (insertar-contacto c (cdr agenda))))))

;; Creamos la funcion  union
(define (union agn1 agn2)
  (if(null? agn1)
     agn2
     (union (cdr agn1) (insertar-contacto (car agn1) agn2))))


;; 1. Usamos tu constructor para crear un par de contactos
(define c1 (contacto "Andres" "Ordoñez" "+56912345678"))
(define c2 (contacto "Renato" "Segura" "+56987654321"))
(define c3 (contacto "Andrea" "Lopez" "+56900000000"))

;; 2. Iniciamos la agenda vacía con el constructor corregido
(define mi-agenda (agenda)) ;; Esto es simplemente '()

;; 3. Poblamos la agenda usando 'cons' (agregamos elementos a la lista)
(define agenda-con-c1 (cons c1 mi-agenda))
(define agenda-con-c1-c2 (cons c2 agenda-con-c1)) 
(define agenda-final (cons c3 agenda-con-c1-c2))

;; Funcion para B
(define (se-llama-andres? c)
  (equal? (car c) "Andres"))

(define mi-promesa-de-conteo (count-lazy agenda-final se-llama-andres?))
(force mi-promesa-de-conteo)


;; Ejemplo para la C

(define busqueda-parcial ((((find agenda-final) "And") "X") "X"))

;; Resultado esperado:
;; Dará verdadero tanto para "Andres" como para "Andrea".
;; Retornará una lista con c1 y c3.


;; Ejemplo para la D (Union)

;; Creamos un contacto nuevo y una "copia" tuya pero con otro numero
(define c4 (contacto "Juan" "Perez" "+56911111111"))
(define c-andres-nuevo (contacto "Andres" "Ordoñez" "+56999999999"))

;; Armamos una segunda agenda
(define agenda-dos (cons c-andres-nuevo (cons c4 (agenda))))

;; Unimos la agenda-final (que tiene a Andres, Renato y Andrea) con la agenda-dos
(define agenda-fusionada (union agenda-final agenda-dos))

;; Si inspeccionas agenda-fusionada, veras que Juan Perez se agrego, 
;; y tu contacto ahora tiene una lista con dos numeros telefonicos diferentes.



;; -------------------- Pregunta 2 -----------------------------------------

;; Para el apartado del contructor del chatbot tendremos que:
(define (chatbot id nombre version modelo)
  (list id nombre version modelo))

;; Para el nivel de los selectores
(define (get-id chatbot)
  (car chatbot))

(define (get-nombre-bot chatbot)
  (cadr chatbot))

(define (get-version chatbot)
  (caddr chatbot))

(define (get-modelo chatbot)
  (cadddr chatbot))

;; Para el nivel de los modificadores, asi con todos, notese que el unico hay 2 parametros de entrada la lista del chatbot y el elemento que se quiere cambiar
(define (set-id id chatbot)
  (list id (get-nombre-bot chatbot) (get-version chatbot) (get-modelo chatbot)))

;;.....

;; TDA TALK
(define (talk mensaje-nuevo mensajes-historicos)
  (list mensaje-nuevo mensajes-historicos))

(define (get-mensaje-nuevo talk)
  (car talk))

(define (get-mensajes-htr talk)
  (cadr talk))


(define (agregar-mensaje m t)
  (talk m (cons m (cons (get-mensaje-nuevo t) (get-mensajes-htr t)))))

(define t1 (talk "Hola" null))
(define t2 (talk "Chao" t1))
(define t3 (agregar-mensaje "HOLA PO PAPI" t2))

;; TDA RESPONSE 
(define (response date str)
  (cons date str))






