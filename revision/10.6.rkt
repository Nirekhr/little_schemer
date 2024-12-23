#lang racket

(define atom?
    (lambda(x)
        (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (- n 1)) (car lat))
      (else (pick (- n 1) (cdr lat))))))

(define index1
  (lambda (a lat n)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) n)
      (else (index1 a (cdr lat) (+ 1 n))))))

(define index
  (lambda (a lat)
    (index1 a lat 1)))

(define build
    (lambda (l1 l2)
        (cons l1 (cons l2 '()))))

(define
  new-entry build)

(define extend-table
  (lambda (entry table)
    (lambda (name)
      (cond
	((member? name (first entry)) (pick (index name (first entry)) (second entry)))
	(else (table name))))))
		

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
      (else (build (quote primitive) e)))))

(define text-of second)

(define *quote
  (lambda (e table)
    (text-of e))) 

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define mk-table
  (lambda (e)
    (extend-table e initial-table)))

(define t2f
  (lambda (l)
    (cond
      ((null? l) initial-table)
      ((> (length l) 1) (extend-table (car l) (t2f (cdr l))))
      (else (mk-table (car l))))))

(define *identifier
  (lambda (e table)
    ((t2f table) e)))

(define *lambda
  (lambda (e table)
    (cons 'non-primitive (cons table (cdr e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)                                                                                                                                                                                                                                                         ((eq? e 'null?) *const)                                                                                                                                                                                                                                                       ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)                                                                                                                                                                                                                                                       ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond 
	 ((eq? (car e) 'quote) *quote)
	 ((eq? (car e) 'lambda) *lambda)
	 ((eq? (car e) 'cond) *cond)
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
        (meaning e '())))

(define table-of first)

(define formals-of second)

(define body-of third)

(define else?
  (lambda (x)
    (equal? 'else x)))

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
  (lambda (I)
    (eq? (first I) 'primitive)))

(define non-primitive?
  (lambda (I)
    (eq? (first I) 'non-primitive)))

(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
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
      ((eq? name (quote add1)) (add1 (first vals)))
      ((eq? name (quote sub1)) (sub1 (first vals)))
      ((eq? name (quote number?)) (number? (first vals))))))

(define apply-closure
  (lambda (closure vals)
    (meaning (third closure) (extend-table (build (second closure) vals) (car closure)))))

(define apply 
  (lambda (fun vals)
    (cond
      ((primitive? fun) (apply-primitive (second fun) vals))
      ((non-primitive? fun) (apply-closure (second fun) vals)))))

(define *application
  (lambda (e table)
    (apply (meaning (car e) table) (evlis (cdr e) table))))

;(define e '(cdr (quote (1 2 3))))
;(define table '())
;(meaning (car e) table)
;(meaning (cdr e) table)
;(define co (meaning '(quote (1 2 3)) table))
;(apply (meaning (car e) table) '((1 2 3)))
;(meaning (car '((1 2 3))) table)
;(evlis '((1 2 3)) table)
;(value '(cons 1 '(1 2)))
;e = (cdr '(1 2 3))
;cdr e=('(1 2 3))
;(apply (primitive cdr) ((1 2 3)))
(value '(car '(1 2)))
(value '(cons 'a '(1 2)))
(value '(cdr '(1 2 3)))
(value '(null? '(1)))
