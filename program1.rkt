#lang racket
;Helper functions used in next and symmetric-difference
(define next1
  (lambda (s1)
    (cond [(null? s1) '()]
          [(equal? '0 (car s1))
           (cons '1 (cdr s1))]
          (else (cons '0 (next1 (cdr s1)))))))

(define union
  (lambda(l1 l2)
    (cond [(null? l1) l2]
          [(member(car l1) l2)
           (union(cdr l1) l2)]
          (else(cons(car l1) (union(cdr l1) l2))))))
;iff takes two truth values and returns iff
(define iff
  (lambda (x y)
    (equal? x y)))

(iff #f #f);

;exlisove-or takes two truth values and returns exlusive-or
(define exclusive-or
  (lambda (x y)
    (not(equal? x y))))

(exclusive-or #t #f);

;difference takes two lists and returns the difference between them
(define difference
  (lambda (s1 s2)
    (cond [(null? s1)'()]
          [(not (member (car s1) s2))
           (cons (car s1) (difference (cdr s1) s2))]
           (else(difference (cdr s1) s2)))))

(difference '(a b c d) '(a c e));

;symmetric-difference takes two lists and returns their symmetric difference
(define symmetric-difference
  (lambda (s1 s2)
    (union (difference s1 s2) (difference s2 s1))))

(symmetric-difference '(a b c) '(b c d e)); 

;stem returns a given list with last element removed
(define (stem s1)
    (cond [(null? (cdr s1))'()]
        (else(cons (car s1) (stem (cdr s1))))))

(stem '(1 2 3 4 5));

;replicate takes an atom and list of atoms
;it searches the list for the atom given and replicates it where ever found
(define replicate 
  (lambda (x s1)
    (cond [(null? s1) '()]
          [(equal? x (car s1)) 
           (append (append (list x) (list x)) (replicate x (cdr s1)))]
          (else(cons (car s1) (replicate x (cdr s1)))))))

(replicate 'fi '(fee fi fo fum fi));

;the function next take the input (reverse (list)) it checks each element until it comes to a zero or the end
;if a 1 is found it gets changed to a 0 
;once a zero is found the function changed the zero to a 1 and returns
;if end of list found returns all 0's
(define next
  (lambda (x)
    (reverse (next1 (reverse x)))))

(next '(0 0 1));
(next '(1 1 0));
(next '(1 1 1 1));
(next '(1 0 1 1 1));

