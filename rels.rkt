#lang racket/base

(require "sets.rkt"
         "pairs.rkt"
         "lists.rkt")
(provide
  fun?
  revrel
  fullfun?)

(define seconds
  (lambda (a-pair)
    (cond
      ((null? a-pair) '())
      (cons (second (car a-pair)) (seconds (cdr a-pair))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel)) (car (car rel))) (revrel (cdr rel)))))))

(define fullfun?
  (lambda (rel)
      (and (fun? rel) (fun? (revrel rel)))))
      

