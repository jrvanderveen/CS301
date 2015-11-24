#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; CSCI 301 Fall 2015 

;; Lab #5

;; 

;; Johan van der Veen 

;; W01067384

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;takes two sets and returns a sigle set (no repeated values)
;;;;;;;;;;;;;;;;;;;;;;;
(define union
  (lambda(l1 l2)
    (cond [(null? l1) l2]
          [(member(car l1) l2)
           (union(cdr l1) l2)]
          (else(cons(car l1) (union(cdr l1) l2)))))) 

;;;;;;;;;;;;;;;;;;;;;;;
;takes two sets and returns a single set of the intersection
;;;;;;;;;;;;;;;;;;;;;;;
(define intersection
  (lambda(l1 l2)
    (cond [(null? l1) '()]
          [(member(car l1) l2)
           (cons(car l1) (intersection(cdr l1) l2))]
          (else(intersection(cdr l1) l2)))))

;;;;;;;;;;;;;;;;;;;;;;;
;difference takes two lists and returns the difference between them
;;;;;;;;;;;;;;;;;;;;;;;
(define difference
  (lambda (s1 s2)
    (cond [(null? s1)'()]
          [(not (member (car s1) s2))
           (cons (car s1) (difference (cdr s1) s2))]
           (else(difference (cdr s1) s2)))))

;;;;;;;;;;;;;;;;;;;;;;;
;symmetric-difference takes two lists and returns their symmetric difference
;;;;;;;;;;;;;;;;;;;;;;;
(define symmetric-difference
  (lambda (s1 s2)
    (union (difference s1 s2) (difference s2 s1))))

;;;;;;;;;;;;;;;;;;;;;;;
;variables
;;;;;;;;;;;;;;;;;;;;;;;
(define variables '(a b c d e f g h i j k l m))
  

;;;;;;;;;;;;;;;;;;;;;;;
;main functions
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;set-value is the main function, it determines the correct function for the list to be passed
;this function may be called multiple times if there are embeded procedures
(define set-value
  (lambda (s1)
    (cond  [(=(length s1) 3) (nullR s1)]
           [(=(length s1) 2) (nullL s1)]
           [(null? (cdr s1)) s1]
           [(not(list? (car s1))) s1]
           (else '(a)))))
;;;;;;;;;;;;;;;;;;;;;;;
;nullL determines if the left most element is null a list or an embeded procedure and directs acordingly
(define nullL
  (lambda (s1)
    (cond [(null?(car s1)) (len3 s1)]
          [(list? (car(car s1)))(set-value (append (list(set-value (car s1))) (cdr s1)))] 
          (else (len3 s1)))))
;;;;;;;;;;;;;;;;;;;;;;;
;nullLR determines if the right most element is null a list or an embeded procedure and directs acordingly
(define nullR
  (lambda (s1)
    (cond [(null? (car (cdr (cdr s1)))) (nullL s1)]
          [(list? (car(car(cdr(cdr s1)))))(set-value(append (append (list(car s1)) (list(car (cdr s1)))) (list(set-value (car(cdr(cdr s1)))))))]
          (else (nullL s1)))))
;;;;;;;;;;;;;;;;;;;;;;;
;if there is a procedure of the tye ((list) oporator (list)) return result
(define len3
  (lambda (s1)
     (cond [(equal?(car (cdr s1)) 'intersection)
            (intersection (car s1) (car (cdr (cdr s1))))]
           [(equal?(car (cdr s1)) 'union)
            (union (car s1) (car (cdr (cdr s1))))]
           [(equal?(car (cdr s1)) 'difference)
            (difference (car s1) (car (cdr (cdr s1))))]
           [(equal?(car (cdr s1)) 'symmetric-difference)
            (symmetric-difference (car s1) (car (cdr (cdr s1))))]
           [(equal?(car (cdr s1)) 'complement)
            (symmetric-difference (car s1) variables)]
           (else '(oporator error)))))



;;;;;;;;;;;tests
(set-value '(((a) union ((b c d) difference (e))) symmetric-difference (m e)))
(set-value '((m) complement ))
(set-value '(() intersection (k l m)))
(set-value '(((a b) union ((b c d) difference (e))) symmetric-difference ((b d e f) union ())))
(set-value '(((a) union (b c)) complement))
(set-value '((a b c d) complement))
(set-value '((a b c d e)))
(set-value '(a b c d e))



     
   