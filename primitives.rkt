#lang racket/base
(provide atom?
	 first
	 second
	 third)

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define first car)
(define second cadr)
(define third caddr)


