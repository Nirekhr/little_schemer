#lang racket/base
(require "primitives.rkt")

(provide
  eqan?
  eqlist?)



(define eqan?
  (lambda (s1 s2)
    (cond
      ((and (number? s1) (number? s2)) (= s1 s2))
      ((or (number? s1) (number? s2)) #f)
      (else (eq? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2))) (and (eqlist? (cdr l1) (cdr l2)) (eqan? (car l1) (car l2))))
      ((and (list? (car l1)) (list? (car l2)))
       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      (else #f))))
      
