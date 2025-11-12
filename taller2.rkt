#lang racket

;; ========================================
;; Taller 2 - Programación Declarativa
;; Wilson David Hernández Martínez 
;; 00135222
;; ========================================

;; ----------------------------------------
;; Ejercicio 1
;; ----------------------------------------

(define (contar-positivos lista)
  (length (filter (lambda (x) (> x 0)) lista)))

(displayln "Ejercicio 1:")
(displayln (contar-positivos '(3 -2 7 0 -5 9)))
(newline)


;; ----------------------------------------
;; Ejercicio 2 
;; ----------------------------------------

(define (cuadrados-pares lista)
  (filter even? (map (lambda (x) (* x x)) lista)))

(displayln "Ejercicio 2:")
(displayln (cuadrados-pares '(1 2 3 4 5 6 7 8)))
(newline)


;; ----------------------------------------
;; Ejercicio 3
;; ----------------------------------------

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln "Ejercicio 3:")
(displayln (factorial 5))
(newline)


;; ----------------------------------------
;; Ejercicio 4
;; ----------------------------------------

(define (elevar-cubo lista)
  (map (lambda (x) (expt x 3)) lista))

(displayln "Ejercicio 4:")
(displayln (elevar-cubo '(2 3 4)))
(newline)


;; ----------------------------------------
;; Ejercicio 5
;; ----------------------------------------

(define (suma-impares lista)
  (foldl + 0 (filter odd? lista)))

(displayln "Ejercicio 5:")
(displayln (suma-impares '(1 2 3 4 5 6 7)))
(newline)


;; ----------------------------------------
;; Ejercicio 6
;; ----------------------------------------

(define (contiene-negativos? lista)
  (ormap (lambda (x) (< x 0)) lista))

(displayln "Ejercicio 6:")
(displayln (contiene-negativos? '(5 9 -3 2)))
(newline)


;; ----------------------------------------
;; Ejercicio 7
;; ----------------------------------------

(define (suma-acumulada lista)
  (let loop ((lst lista)
             (acc '())
             (suma 0))
    (if (null? lst)
        (reverse acc)
        (let ((nuevo (+ suma (car lst))))
          (loop (cdr lst) (cons nuevo acc) nuevo)))))


(displayln "Ejercicio 7:")
(displayln (suma-acumulada '(1 2 3 4)))
(newline)


;; ----------------------------------------
;; Ejercicio 8
;; ----------------------------------------

(define (concatenar-cadenas lista)
  (foldr string-append "" lista))

(displayln "Ejercicio 8:")
(displayln (concatenar-cadenas '("Hola" " " "Mundo")))
(newline)


;; ----------------------------------------
;; Ejercicio 9
;; ----------------------------------------

(define (doble-mayores-que5 lista)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lista)))

(displayln "Ejercicio 9:")
(displayln (doble-mayores-que5 '(3 6 8 2 10)))
(newline)


;; ----------------------------------------
;; Ejercicio 10
;; ----------------------------------------

(define (invertir-lista lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))

(displayln "Ejercicio 10:")
(displayln (invertir-lista '(1 2 3 4)))
(newline)


;; ----------------------------------------
;; Ejercicio 11
;; ----------------------------------------

(define (aplicar-a-lista f lista)
  (map f lista))

(define (cuadrado x) (* x x))

(displayln "Ejercicio 11:")
(displayln (aplicar-a-lista cuadrado '(1 2 3 4)))
(newline)


;; ----------------------------------------
;; Ejercicio 12
;; ----------------------------------------

(define (promedio-mayores-5 lista)
  (let* ((mayores (filter (lambda (x) (> x 5)) lista))
         (suma (foldl + 0 mayores))
         (cantidad (length mayores)))
    (if (> cantidad 0)
        (/ (exact->inexact suma) cantidad)
        0)))

(displayln "Ejercicio 12:")
(displayln (promedio-mayores-5 '(3 8 10 4 9 2 7)))
(newline)

