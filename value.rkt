#lang racket/base

(require "primitives.rkt")

(provide
  atom-to-action
  list-to-action
  initial-table
  *identifier
  *const
  *quote
  *lambda
  *cond
  *application
  apply-primitive
  value)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define sub1
  (lambda (n)
    (- n 1)))

(define add1
  (lambda (n)
    (+ n 1)))

(define value
  (lambda (e)
    (meaning e '())))

(define build
  (lambda (l1 l2)
      (cons l1 (cons l2 '()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      ((eq? e 'atom?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
	 ((eq? 'lambda (car e)) *lambda)
	 ((eq? 'quote (car e)) *quote)
	 ((eq? 'cond (car e)) *cond)
	 (else *application)))
      (else *application))))


(define *quote
  (lambda (e table)
    (second e)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define look-in-entry-help
  (lambda (name names values table-f)
    (cond
      ((null? names) (table-f name))
      ((eq? name (car names)) (car values))
      (else (look-in-entry-help name (cdr names) (cdr values) table-f)))))

(define look-in-entry
  (lambda (name entry table-f)
    (look-in-entry-help name (car entry) (second entry) table-f)))

(define look-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (look-in-entry name (car table) (lambda (name) (look-in-table name (cdr table) table-f)))))))

(define initial-table
  (lambda (x)
    (car '())))

(define *identifier
  (lambda (name table)
    (look-in-table name table initial-table)))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(define *application
  (lambda (e table)
    (apply (meaning (car e) table) (evlis (cdr e) table))))

(define primitive?
  (lambda (e)
    (eq? 'primitive (car e))))

(define non-primitive?
  (lambda (e)
    (eq? 'non-primitive (car e))))

(define evlis
  (lambda (e table)
    (cond
      ((null? e) '())
      (else (cons (meaning (car e) table) (evlis (cdr e) table))))))

(define apply
  (lambda (func arg)
    (cond
      ((primitive? func) (apply-primitive (second func) arg))
      ((non-primitive? func) (apply-closure (second func) arg)))))

(define apply-primitive 
  (lambda (func arg)
    (cond
      ((eq? func 'cons) (cons (car arg) (second arg)))
      ((eq? func 'eq?) (eq? (car arg) (second arg)))
      ((eq? func 'car) (car (car arg)))
      ((eq? func 'cdr) (cdr (car arg)))
      ((eq? func 'null?) (null? (car arg)))
      ((eq? func 'atom?) (:atom? (car arg)))
      ((eq? func 'number?) (number? (car arg)))
      ((eq? func 'zero?) (zero? (car arg)))
      ((eq? func 'add1) (add1 (car arg)))
      ((eq? func 'sub1) (sub1 (car arg))))))

(define :atom?
  (lambda (e)
    (cond
      ((atom? e) #t)
      ((list? e) #f)
      ((eq? e 'non-primitive) #t)
      ((eq? e 'primitive) #t)
      (else #f))))


(define apply-closure
  (lambda (func arg)
      (meaning (third func) (cons (build (second func) arg) (first func)))))

(define *cond
  (lambda (e table)
      (evcon (cdr e) table)))

(define evcon
  (lambda (e table)
    (cond
      ((else? (car (car e))) (meaning (second (car e)) table))
      ((meaning (car (car e)) table) (meaning (second (car e)) table))
      (else (evcon (cdr e) table)))))

(define else?
  (lambda (x)
    (cond
    ((atom? x) (eq? x 'else))
    (else #f))))

 












