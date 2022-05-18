#lang scheme

;; n primeros términos Fibonacci
(define fibonacci
  (lambda (n)
    (let ([a 1]
          [b 1])
      (fib a b n))))

(define fib
  (lambda (a b n)
    (if (zero? n)
        '()
        (cons a (fib b (+ a b) (sub1 n))))))

(define my-map
  (lambda (fn lst)
    (if (null? lst)
        '()
        (cons (fn (car lst)) (my-map fn (cdr lst))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (lst)
    (cond
      [(null? lst) #t]
      [(atom? (car lst)) (lat? (cdr lst))]
      [else #f])))

(define lat2?
  (lambda (lst)
    (or (null? lst)
        (and (atom? (car lst))
             (lat2? (cdr lst))))))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? a (car lat))
                (member? a (cdr lat)))])))

(define member2?
  (lambda (a lat)
    (and (not (null? lat))
         (or (eq? a (car lat))
             (member? a (cdr lat))))))

(define r-member?
  (lambda (a expr)
    (cond
      [(null? expr) #f]
      [(atom? expr) (eq? a expr)]
      [else (or (r-member? a (first expr))
                (r-member? a (cdr expr)))])))


(define r-member2?
  (lambda (a expr)
    (and (not (null? expr))
         (or (and (atom? expr) (eq? a expr))
             (r-member2? a (first expr))
             (r-member2? a (cdr expr))))))

(define node
  (lambda (val l r)
    (list val l r)))

(define (key node) (car node))
(define (left node) (cadr node))
(define (right node) (caddr node))

(define tree-contains?
  (lambda (val tree)
    (and (not (empty? tree))
         (or (equal? (key tree) val)
             (tree-contains? val (left tree))
             (tree-contains? val (right tree))))))

(define tree-size
  (lambda (tree)
    (if (empty? tree)
        0
        (+ 1
           (tree-size (left tree))
           (tree-size (right tree))))))
