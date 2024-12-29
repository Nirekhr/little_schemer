#lang racket/base

(require "primitives.rkt")
(provide lat?
         member?
         rember
         insertR
         insertL
         subst
         subst2
         multirember
         multiinsertR
         multiinsertL
         multiinsertLR
         pick
         rempick
         no-nums
         all-nums
         occur
         looking)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define insertR
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l)) (cons old (cons new (cdr l))))
      (else (cons (car l) (insertR new old (cdr l)))))))
	     
(define insertL
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l)) (cons new (cons old (cdr l))))
      (else (cons (car l) (insertL new old (cdr l)))))))

(define multiinsertL
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l)) (cons new (cons old (multiinsertL new old (cdr l)))))
      (else (cons (car l) (multiinsertL new old (cdr l)))))))

(define multiinsertR
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l)) (cons old (cons new (multiinsertR new old (cdr l)))))
      (else (cons (car l) (multiinsertR new old (cdr l)))))))

(define subst
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? old (car l)) (cons new (cdr l)))
      (else (cons (car l) (subst new old (cdr l)))))))

(define subst2
  (lambda (new o1 o2 l)
    (cond
      ((null? l) '())
      ((eq? o1 (car l)) (cons new (cdr l)))
      ((eq? o2 (car l)) (cons new (cdr l)))
      (else (cons (car l) (subst2 new o1 o2 (cdr l)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (- n 1)) (car lat))
      (else (pick (- n 1) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (- n 1)) (cdr lat))
      (else (cons (car lat) (rempick (- n 1) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((not (number? (car lat))) (all-nums (cdr lat)))
      (else (cons (car lat) (all-nums (cdr lat)))))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? a (car lat)) (+ 1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a n lat)
    (cond
      ((number? n) (keep-looking a (pick n lat) lat))
      (else (eq? n a)))))
  


