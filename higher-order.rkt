#lang racket/base

(provide
  eq-c
  rember-f
  insertL-f
  insertR-f
  insert-g)

(define rember-f
  (lambda (f)
    (lambda (a l)
      (cond
	((null? l) '()) ((f (car l) a) (cdr l))
	(else (cons (car l) ((rember-f f) a (cdr l))))))))

(define eq-c
  (lambda (a1)
    (lambda (a2)
      (eq? a1 a2))))

(define insertL-f
  (lambda (f)
    (lambda (new old l)
      (cond
	((null? l) '())
	((f (car l) old) (cons new (cons old (cdr l))))
	(else (cons (car l) ((insertL-f f) new old (cdr l))))))))

(define insertR-f
  (lambda (f)
    (lambda (new old l)
      (cond
	((null? l) '())
	((f (car l) old) (cons old (cons new (cdr l))))
	(else (cons (car l) ((insertR-f f) new old (cdr l))))))))

(define insert-g
  (lambda (f g)
    (lambda (new old l)
      (cond
	((null? l) '())
	((f (car l) old) (g new old (cdr l)))
	(else (cons (car l) ((insert-g f g) new old (cdr l))))))))
			   
