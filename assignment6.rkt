#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; CSCI 301 Fall 2015 

;; Lab #5

;; 

;; Johan van der Veen 

;; W01067384

;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;empty set to populate
(define sd '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;if the element is not a list aka single element then simply check if its in the set with member
;else call search
(define size
  (lambda (e s1)
    (cond [(not(list? e)) (not(not(member e s1)))]
          (else (search e s1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;if s1 ever becomes null no match was found return #f
;iterate through the whole set given by calling validate on each element of s1
;if no match found on car then search cdr of s1
;if match found #t exit
(define search
  (lambda (e s1)
    (cond [(null? s1) #f]
          [(not(validate e (car s1))) (search e (cdr s1))]
          (else #t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;if s1 is still a list call setsd on ssd of sd (s1 is know a single element of s1)
;sd is the set-symetric difference of the two elements
;this handles nested lists eg:'(a (((b c))) d) '(d (((c b))) a)) ssd of this = '((((c b))) (((b c)))) then recursivly again '()

(define validate
  (lambda (e s1)
    (if (list? s1) (setsd e (set! sd (set-symmetric-difference e s1))) #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;if the symetric difference between s1 and e is '() then match found
;if there arnt only two elements in sd then there is a mismatch of formats and wont find a match with this element of s1
;if either the first or seccond element is not a list then match wont be found
;other wise if the first and seccond element have the same size then one layer of the nested loops will have been taken care of call search
;other wise match wont be found
(define setsd
  (lambda (e s1)
    (cond [(null? sd) #t]
          [(not(= (length sd) 2)) #f]
          [(not (list? (car sd))) #f]
          [(not (list? (car (cdr sd)))) #f]
          [(equal? (length (car sd)) (length (car (cdr sd)))) (search (car sd) (cdr sd))]
          (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;input an element to be tested aginst a set
;if the element is null #t else call size
(define member?
  (lambda (e s1)
    (if (not(null? e)) (size e s1) #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(member? 'a '(a b c))
(member? 'a '(b (a) c))
(member? '(a) '(b (a) c))
(member? '(a) '(b a c))
(member? 'a '((b a c)))
(member? '(a b c) '(a b c))
(member? '(a b c) '((c b a)))
(member? '(a (((b c))) d) '((d (((c b))) a)))
(member? '(a (a b c) d) '(a (d (c b a) a) c))
(member? '(a b) '(b a (b c a (c b a))))
  
  
  
  
