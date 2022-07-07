#lang racket
;Xavier Rodirguez
;CS301
;LAB 8

;HELPERS

;checks if it is an atom and not a list.
(define atom?
  (lambda (a)
    (not (list? a))))

; Realizes the truth function implies
(define implies
  (lambda (S1 S2)
    (or (not S1) S2)))

; Realizes the truth function  iff
(define iff
  (lambda (S1 S2)
    (eq? S1 S2)))

;Removes the duplicate variables 
(define (remove-duplicates L)
    (cond ( (null? L) '() )
          ( (member (car L) (cdr L)) (remove-duplicates (cdr L)) )
          (else
           (cons (car L) (remove-duplicates (cdr L))))
          ))
;removes the and or not iff implies opps 
(define (remove-opps L)
    (cond ((null? L) '())
          ( (member (car L) '(and or not iff implies #t #f))
            (remove-opps (cdr L)))
          (else (cons (car L) (remove-opps (cdr L))))))

;What this function does it takes the list and two args and
;makes substitute for where the args show up in the list.
(define (Substitute L a1 a2)
  (cond
    ((null? L) '())
    ((list? (car L))
     (cons (Substitute (car L) a1 a2)(Substitute (cdr L) a1 a2)))
    ((eq? (car L) a1)(cons a2 (Substitute (cdr L) a1 a2)))
    
    (else
     (cons (car L)
     (Substitute(cdr L) a1 a2)))))

;Collect prop varibles removes the opps and, or, implies and iff.
;and leaves us with the other varibles within the list.
(define collect-prop-variables
  (lambda (L)
    (remove-duplicates
     (remove-opps
      (collect-prop-variables-helper L)))))

(define collect-prop-variables-helper
  (lambda (L)
    (cond ( (null? L) '())
          ( (atom? (car L))
                   (cons (car L) (collect-prop-variables-helper (cdr L))))
          (else
           (append (collect-prop-variables-helper (car L))
                                           (collect-prop-variables-helper
                                            (cdr L))))
          )))


;MAIN

;evaluates the string and does the actual logic for the and, or, implies and iff opperators.
(define (evaluate-wff W)
    (cond ((atom? W) W)
          ((= (length W) 1) (evaluate-wff (car W)))
          ((= (length W) 2) (not (evaluate-wff (cdr W))))
          (else
           (cond
             ((eq? (cadr W) 'or) (or (evaluate-wff (car W)) (evaluate-wff (cddr W))))
             ((eq? (cadr W) 'and) (and (evaluate-wff (car W)) (evaluate-wff (cddr W))))
             ((eq? (cadr W) 'implies) (implies (evaluate-wff (car W)) (evaluate-wff (cddr W))))
             ((eq? (cadr W) 'iff) (iff (evaluate-wff (car W)) (cddr W))))
             )))


;This function is Substituting the varibles given by the problem with t and f,
;returns with true or false if the evaluated list is a tautology or not. 
(define (tautology? L)
  (let ((var (collect-prop-variables L)))
    (cond
      ((null? var) (evaluate-wff L))
      (else
       (and (tautology? (Substitute L (car var) #t)) (tautology? (Substitute L (car var) #f)))
       )
      )
    )
  ) 

(tautology? '(p or (not p)))
(tautology? '((not p) implies (p implies q))) 
(tautology? '(p or q))
(tautology? '(p and (r implies (not q))))
(tautology? '((p and (not q)) or ((not s) and r)))

