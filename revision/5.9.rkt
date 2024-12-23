#lang racket

(define multiup
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (car l)) (multiup (cdr l)))
      ((null? (cdr (car l)))(cons (car (car l)) (multiup (cdr l))))
      (else (cons (car l) (multiup (cdr l)))))))

(define x 'comma)
(define y 'dot)
(define a 'kiwis)
(define b 'plums)
(define lat1 '(bananas kiwis))
(define lat2 '(peaches apples bananas))
(define lat3 '(kiwis pears plums bananas cherries))
(define lat4 '(kiwis mangoes kiwis guavas kiwis))
(define l1 '((curry) ( ) (chicken) ( )))
(define l2 '((peaches) (and cream)))
(define l3 '((plums) and (ice) and cream))
(define l4 '())
(multiup l4) 
(multiup l1) 
(multiup l2)
