#lang racket

(define multisubst-k
  (lambda (new old lat k)
    (cond
      ((null? lat) (k '()))
      ((eq? (car lat) old) (multisubst-k new old (cdr lat) (lambda (d) (k (cons new d)))))
      (else (multisubst-k new old (cdr lat) (lambda (d) (k (cons (car lat) d))))))))

(lambda (d) ((lambda (d) ((lambda (d) ((lambda (x) x) (cons 'a d))) (cons 'x d))) (cons 'c d))) 
#|this is the final version of k if 
(define new 'x) 
(define old 'b) 
(define lat '(a b c)) 
(define k (lambda (x) x)) 
