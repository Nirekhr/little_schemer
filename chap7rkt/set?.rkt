#lang racket

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
	((member (car lat) (cdr lat)) #f)
	(else (set (cdr lat))))))

(define lat '(apple is apple tree))

(set? lat)
