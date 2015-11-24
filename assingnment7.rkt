#lang racket
;;;;;;;;;;helper;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define check-final
  (lambda (current dfa)
    (cond [(member current (car dfa)) #t]
          (else #f))))

(define eval-move
  (lambda (input current dfa)
    (cond [(null? dfa) (exit #f)] 
          [(equal? current (car (car dfa))) (move input (cdr (car dfa)))]
          (else (eval-move input current (cdr dfa))))))

(define move
  (lambda (input dfa-move)
    (cond [(equal? input (car (car dfa-move))) (car (cdr (car dfa-move)))]
          (else (move input (cdr dfa-move))))))
;;;;;;;;;;main;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define accepted?
  (lambda (input current dfa)
    (cond[(null? input)(check-final current dfa)]
         (else (accepted? (cdr input)(eval-move (car input) current (cdr dfa)) dfa)))))


;;;;;;;;;;test cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accepted? '() 7 '((7) (7 (a 12) (b 7)) (12 (a 7) (b 12))))
(accepted? '() 12 '((7) (7 (a 12) (b 7)) (12 (b 7) (a 12))))
(accepted? '(a b a b) 0 '((0) (0 (a 0) (b 0))))
(accepted? '(a b a b) 0 '((0) (0 (a 1) (b 1)) (1 (a 1) (b 1))))
(accepted? '(a b a a b a a a) 1 '((1 2 3) (1 (a 0) (b 2)) (2 (a 0) (b 3)) (3 (a 0) (b 0)) (0 (a 0) (b 0))))
(accepted? '(b b) 1 '((1 2 3) (1 (a 0) (b 2)) (2 (a 0) (b 3)) (3 (a 0) (b 0)) (0 (a 0) (b 0))))
(accepted? '(b b b) 1 '((1 2 3) (1 (a 0) (b 2)) (2 (a 0) (b 3)) (3 (a 0) (b 0)) (0 (a 0) (b 0))))