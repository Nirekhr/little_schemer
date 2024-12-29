#lang racket/base

(require "primitives.rkt")
(provide
  a-pair?
  first
  second
  build
  revpair
  shift
  weight*
  length*
  shuffle)

(define a-pair?
  (lambda (s)
    (cond
      ((atom? s) #f)
      ((null? s) #f)
      ((null? (cdr s)) #f)
      ((null? (cdr (cdr s))) #t)
      (else #f))))

(define first car)

(define second
  (lambda (x)
    (car (cdr x))))

(define build
  (lambda (l1 l2)
    (cons l1 (cons l2 '()))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define shift
  (lambda (p)
    (build (car (car p)) (build (second (car p)) (second p)))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (+ (* (weight* (first pora)) 2) (weight* (second pora)))))))

 (define length*
   (lambda (pora)
     (cond
       ((atom? pora) 1)
       (else (+ (length* (first pora)) (length* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora) (shuffle (second pora)))))))


