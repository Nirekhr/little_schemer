#lang racket

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define member? 
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define set?
  (lambda (l)
    (and (lat? l) (set?1 l))))

(define set?1
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))



(define e1
  '((lambda (x)
     (cond
       ((atom? x) 'done)
       ((null? x) 'almost)
       (else 'never))) '_____))

(define e2
  '(lambda (x y)
      (lambda (u)
	(cond
	  (u x)
	  (else y)))))

(define e3
  '((lambda (x)
    ((lambda (x)
      (add1 x))
      (add1 4))) 6))

(define e4 '(3 'a 'b))
(define e5 '(lambda (lat) (cons 'lat lat)))
(define e6 '(lambda (lat (lyst)) a 'b))

(define union
  (lambda (l1 l2)
    (cond
    ((null? l1) l2)
    (else (union (cdr l1) (cons (car l1) l2))))))


(define *lambda?2
  (lambda (f c)
    (cond
      ((eq? (car f) 'lambda) (*lambda?2 (third f) (union (second f) c)))
      (else c))))

(define *lambda?1
  (lambda (f)
    (*lambda?2 f '())))

(define member*
  (lambda (a l) 
    (cond 
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define multimember*
  (lambda (S lat)
    (cond
      ((null? lat) #t)
      (else (or (member* (car S) lat) (multimember* (cdr S) lat))))))

(define *quote?
  (lambda (qe)
    (and (eq? 2 (length qe)) (eq? 'quote (car qe)))))

(define *lambda?
  (lambda (f)
      (and
	(set? (second f))
	(multimember* (third f) (union '(cons lambda else car cdr null? eq? #t #f atom? zero? add1 sub1 number? quote cond) (*lambda?1 f))))))

(define *cond1
  (lambda (ce)
    (cond
      ((null? ce) #t)
      ((eq? (length (car ce)) 2)
       (cond
	 ((eq? (car (car ce)) 'else)
	  (cond
	    ((null? (cdr ce)) #t)
	    (else #f)))
	 (else (*cond1 (cdr ce)))))
      (else #f))))

(define *cond?
  (lambda (ce)
    (and (eq? (car ce) 'cond) (*cond1 (cdr ce)))))

(*lambda? e5)
(*lambda? e6)
(*lambda? e2)




