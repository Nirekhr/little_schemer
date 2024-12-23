#lang racket

(define atom?
    (lambda(x)
        (and (not (pair? x)) (not (null? x)))))

(define build
    (lambda (l1 l2)
        (cons l1 (cons l2 (quote ())))))

(define
  new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table) (lambda (name) (lookup-in-table name (cdr table) table-f)))))))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e)))

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (lambda (vals) (meaning (body-of e (extend-table (new-entry (formals-of e) vals) table)))))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
	 ((eq? (car e) (quote quote)) *quote)
	 ((eq? (car e) (quote lambda)) *lambda)
	 ((eq? (car e) (quote cond)) *cond)
	 (else *application)))
	 (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define value
    (lambda (e)
        (meaning e (quote ()))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define else?
  (lambda (x)
    (equal? (quote else) x)))

(define question-of first)
(define answer-of second)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define *cond
  (lambda (e table)
    (evcon (cdr e) table)))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else (cons (meaning (car args) table) (evlis (cdr args) table))))))

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))


(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))


(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive)) #t)
      ((eq? (car x) (quote non-primitive) #t))
      (else #f))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons)) (cons (first vals) (second vals)))
      ((eq? name (quote car)) (car (first vals)))
      ((eq? name (quote cdr)) (cdr (first vals)))
      ((eq? name (quote null?)) (null? (first vals)))
      ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
      ((eq? name (quote atom?)) (:atom? (first vals)))
      ((eq? name (quote zero?)) (zero? (first vals)))
      ((eq? name (quote add1)) (+ 1 (first vals)))
      ((eq? name (quote sub1)) (- (first vals) 1))
      ((eq? name (quote number?)) (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (cdr (cdr closure)) vals)))

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive fun vals))
      ((non-primitive? fun) (apply-closure fun vals)))))

(define *application
  (lambda (e table)
    (apply (meaning (car e) table) (evlis (cdr e) table))))


(value ((lambda (x) (add1 x)) 2))
;e = (cdr '(1 2 3))
;cdr e=('(1 2 3))
;(apply (primitive cdr) ((1 2 3)))

