#lang racket

(define multiinsertR
 (lambda (new old lat)
  (cond
  ((null? lat) '())
  (else (cond
  ((eq? (car lat) old)
   (cons old (cons new (multiinsertR new old (cdr lat)))))
  (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))

(define lat '(Hi Bad Hi))
(define new 'Jim)
(define old 'Hi)

(multiinsertR new old lat)