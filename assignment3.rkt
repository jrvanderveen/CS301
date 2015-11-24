#lang racket
;dupla duplicates an occurence of a given element in a given set
(define dupla
  (lambda (x s1)
    (cond [(null? s1) '()]
          (else(cons x (dupla x (cdr s1)))))))

(dupla 'a '(c d e))
(dupla 'b '(a b c d))
;Number-Of-Occurences returns the amount of times an element shows up in the given set
(define Number-Of-Occurances
  (lambda (x s1)
    (cond [(null? s1) 0]
          [(equal? x (car s1))
           (+ 1 (Number-Of-Occurances x (cdr s1)))]
           (else(Number-Of-Occurances x (cdr s1))))))

(Number-Of-Occurances '2 '(2 3 4 2 2 2))
(Number-Of-Occurances '0 '(1 3))
(Number-Of-Occurances '1 '( ))

;implies returns the truth value of x implies y
(define implies
  (lambda (x y)
    (cond [(equal? x y)]
          [(equal? x #t) #f]
          (else #t))))

(implies #t #t)
(implies #t #f)
(implies #f #t)
(implies #f #f)