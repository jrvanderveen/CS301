#lang racket
;;;;;;;;;;;;;;;
(define term-abc '((4) (1 (a 1 2) (b 1) (c 1)) (2 (b 3)) (3 (c 4))))
(define mach2 '((3) (0 (a 1) (eps 2)) (1 (b 3)) (2 (a 2 3))))
;;;;;;;;;;;;;;;
(define transitions
  (lambda (st sy mach)
    (cond [(null? st) '()]
          [(null? sy) '()]
          [(null? mach) '()]
          (else (find_tran st sy  mach)))))

(define find_tran 
  (lambda (st sy mach)
    (cond [(null? mach) '()]
          [(equal?(car (car mach)) st)(return_trans sy (cdr (car mach)))]
          (else (find_tran st sy (cdr mach))))))

(define return_trans
  (lambda (sy mach)
    (cond [(null? mach)'()]
          [(equal? (car (car mach)) sy)(cdr(car mach))]
          (else (return_trans sy (cdr mach))))))           
;;;;;;;;;;;;;
(define nfa-execute
 (lambda (string st mach)
   (cond [(null? string) '()]
         [(null? st) '()]
         [(null? mach) '()]
         (else (append '() (search_path string st (cdr mach) (car mach)))))))
         
(define search_path
  (lambda (string st mach accepting)
    (cond [(member st accepting) #t]
          [(null? string)  #f]
          [(member st accepting) #t]
          (else (backtracker string st (transitions st (car string) mach) mach accepting)))))
          
(define backtracker
 (lambda (string st trans mach accepting)
   (cond [(null? trans) #f]
         [(equal? (search_path (cdr string) (car trans) mach accepting) #f) (backtracker string st (cdr trans) mach accepting)]
         (else #t))))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;find epsilon transition for given state
;;;;;once no more trans check epsilon trans and dont consume a letter if eps used
(nfa-execute '(a) 0 mach2)

