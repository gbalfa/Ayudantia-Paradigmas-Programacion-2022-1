#lang racket

;; val en '(a b c d e) ?
;; (r-member? 'c '(a b c d)) -> #t
(define member?
  (lambda (val lst)
    (and (not (null? lst))
         (or (equal? val (car lst))
             (member? val (cdr lst))))))

;; val en '(a b c d (e f g)) ?
;; (r-member? 'k '(a b c d (e f (g h) (i (j k))))) -> #t
(define r-member?
  (lambda (val expr)
    (cond
      [(null? expr) #f]
      [(list? (car expr))
       (or (r-member? val (car expr))
           (r-member? val (cdr expr)))]
      [#t (or (equal? val (car expr))
              (r-member? val (cdr expr)))])))

;; versión mejorada
;; val en '(a b c d (e f g)) ?
;; (r-member2? 'k '(a b c d (e f (g h) (i (j k))))) -> #t
(define r-member2?
  (lambda (val expr)
    (cond
      [(null? expr) #f]
      [(not (list? expr)) (equal? val expr)]
      [else (or (r-member2? val (car expr))
                (r-member2? val (cdr expr)))])))

(define predicado-t #t)
(define predicado-f #f)
(define consecuencia-t 0)
(define consecuencia-f 1)

(cond
  [predicado-f consecuencia-t]
  [predicado-f consecuencia-f]
  [#t 100])
