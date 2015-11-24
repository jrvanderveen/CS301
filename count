#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; CSCI 301 Fall 2015 

;; Lab #4 

;; 

;; Johan van der Veen 

;; W01067384

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;function 1
;define elements for count to compare to 
(define reference '(and or xor implies not iff))
;look through list and all its sub-lists if there are any
;if the function finds a value that is not an expression ie a variable record it
(define (cpv seq)
  (cond [(null? seq) '()]
        [(list? (car seq)) (append (cpv (car seq)) (cpv (cdr seq)))]
        [(not (member (car seq) reference))  
         (append (list (car seq)) (cpv (cdr seq)))]
        (else(cpv (cdr seq)))))
;return a list with no duplicated values 
(define (collect-prop-variables final)
  (remove-duplicates (cpv final)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;function 2
;look through list an all its sub-lists if there are any
;user will give an element to look for and one to replace said element with
(define (substitute seq x y)
    (cond [(null? seq) '()]
        [(list? (car seq)) (cons (substitute (car seq)x y) (substitute (cdr seq)x y))]
        [(equal? (car seq) x)  
         (append (list y) (substitute (cdr seq)x y))]
        (else(append (list(car seq)) (substitute (cdr seq)x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;test cases
(substitute '(not ((B and A) or (A implies B))) 'B '(#t and X))
(collect-prop-variables'((B iff (A xor D)) implies (not (C implies D))))
(collect-prop-variables '(A and (not A)) ) 
(substitute '(C or (D xor D)) 'D #f)