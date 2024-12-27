#lang racket/base

(require "primitives.rkt")

(provide
  value
  numbered?)

(define ^
  (lambda (n1 n2)
    (cond
    ((zero? n2) 1)
    (else (* n1 (^ n1 (- n2 1)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp)
       (cond
	 ((number? aexp) #t)
	 (else #f)))
      (else (and (numbered? (car aexp)) (numbered? (caddr aexp)))))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      ((eq? '+ (cadr aexp)) (+ (value (car aexp)) (value (caddr aexp))))
      ((eq? 'x (cadr aexp)) (* (value (car aexp)) (value (caddr aexp))))
      ((eq? '^ (cadr aexp)) (^ (value (car aexp)) (value (caddr aexp)))))))


