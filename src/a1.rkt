#lang scheme

;; Función que calcula el factorial de un número entero positivo
;; 1...n
(define (factorial n)
  (fact 1 n))

(define (fact actual fin)
  (if (equal? actual fin)
      fin
      (* actual (fact (+ 1 actual) fin))))

;; Función factorial usando recursividad de cola (tail recursion)
;; 1...n
(define (factorial-cola n)
  (fact-cola 1 n 1))

(define (fact-cola actual fin acc)
  (if (equal? actual fin)
      (* actual acc)
      (fact-cola (+ actual 1) fin (* actual acc))))

;; Función que calcula el factorial de un número entero positivo
;; n...1
(define (factorial2 n)
  (if (equal? n 1)
      1
      (* n (factorial2 (- n 1)))))

;; Función factorial usando recursividad de cola (tail recursion)
;; n...1
(define (factorial-cola2 n)
  (fact-cola2 n 1))

(define (fact-cola2 n acc)
  (if (equal? n 1)
      acc
      (fact-cola2 (- n 1) (* acc n))))

(provide factorial)
(provide factorial-cola)
(provide factorial2)
(provide factorial-cola2)
