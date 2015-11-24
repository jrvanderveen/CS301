#lang racket
;takes a list of elements and returns a set (aka the list with no repititions)
(define (list-to-set l)
    (cond [(null? l) '()]
          [(member(car l)(cdr l))
           (list-to-set(cdr l))]
          (else(cons(car l) (list-to-set(cdr l))))))
;takes two sets and returns a sigle set (no repeated values)
(define union
  (lambda(l1 l2)
    (cond [(null? l1) l2]
          [(member(car l1) l2)
           (union(cdr l1) l2)]
          (else(cons(car l1) (union(cdr l1) l2))))))    
;takes two sets and returns a single set of the intersection
(define intersection
  (lambda(l1 l2)
    (cond [(null? l1) '()]
          [(member(car l1) l2)
           (cons(car l1) (intersection(cdr l1) l2))]
          (else(intersection(cdr l1) l2)))))