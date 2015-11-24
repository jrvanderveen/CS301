#lang racket
;;;;;;;;;;helper functions;;;;;;;;
(define itterate
  (lambda (e1 s1)
    (cond [(null?  (cdr s1)) #t]
          (else(set! preserve-s1 s1)
               (if (search-l (car e1)(cdr e1) s1) 
                   (if (search-r (cdr e1)(car e1) s1) (itterate (car s1) (cdr s1)) #f) #f)))))

(define search-l
  (lambda (e-l e-r s1)
    (cond [(null? s1)#t]
          [(equal (cons e-l e-r) (car s1)) (search-l e-l e-r (cdr s1))]
          [(member e-l (cdr (car s1))) (match (cons (car (car s1)) e-r)preserve-s1)]
          (else (search-l e-l e-r (cdr s1))))))
          
(define search-r
  (lambda (e-r e-l s1)
    (cond [(null? s1) #t]
          [(equal (cons e-l e-r) (car s1)) (search-r e-r e-l (cdr s1))]
          [(member e-r (list(list(car (car s1))))) (match (cons e-l (cdr (car s1))) preserve-s1)]
          (else (search-r e-r e-l (cdr s1))))))

(define match 
  (lambda (pair preserve-s1)
    (cond [(member pair preserve-s1) #t]
          (else #f))))

(define equal
  (lambda (pair pair2)
    (cond [(equal? pair pair2) #t]
          [(equal? (reverse pair) pair2) #t]
          (else #f))))

(define preserve-s1 '())
;;;;;;;;;;main;;;;;;;;;;;;;;;;;;;;
(define transitive?
  (lambda (s1)
    (cond [(null? s1) #t]
          (else(itterate (car s1) (cdr s1))))))

;;;;;;;;;;test cases;;;;;;;;;;;;;;
(transitive? '())
(transitive? '((a a) (b b) (c c) (d d) (a b) (b a)))
(transitive? '((a c) (a b) (b c) (c d)))
(transitive? '((a b)(b c)(s s)(d b)(a c)(d c)))
(transitive? '((a b)(b c)(d b)(a c)))