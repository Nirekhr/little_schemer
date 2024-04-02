#lang racket

(define Fapply
  (lambda (f x)
    (cond
      ((null? f) '?)
      ((equal? (car (car f)) x) (car (cdr (car f))))
      (else (Fapply (cdr f) x)))))

(define r1 '((a b) (a a) (b b)))
(define r2 '((c c)))
(define r3 '((a c) (b c)))
(define r4 '((a b) (b a)))
(define f1 '((a 1) (b 2) (c 2) (d 1)))
(define f2 '())
(define f3 '((a 2) (b 1)))
(define f4 '((1 $) (3 *)))
(define d1 '(a b))
(define d2 '(c d))
(define x 'a)

(Fapply f1 x)
(Fapply f2 x)
(Fapply f3 x)

