#lang racket
;Xavier Rodriguez
;CS301
;LAB6

;FUNCTIONS

;cuts the list until its empty and
;if the list passes through then it is reflexive
;issue i had was no covering the case b a, a b
(define (Reflexive? L S)
  (cond
    ((and (empty? L) (empty? S)) #t);
    ((empty? L) #f)
    (else (if (equal? (car (car L)) (car S))
              (Reflexive? (cdr L) (cdr S)) #f))))


;cuts through the list and if the list passes
;then it is symmetric same as reflexive 
(define (Symmetric? L)
 (cond
     ((empty? L) #t)
     (else(if
           (equal? (car(car L)) (car(cdr(car L)))) (Symmetric? (cdr L))
          (if
           (and (equal? (car(car L)) (car(cdr(car(cdr L)))))
             (equal? (car(cdr(car L))) (car(car(cdr L)))))
           (Symmetric? (cdr(cdr L))) #f)
     ))))


;transativie doesent work i wanted to figure out that i could index each pair
;so then i can say that a is less than b but i ran out of time.
(define (Transitive? L)
  (let ((index (car L)))
  (cond
     ((empty? L) #t)
     (else
      (equal? index (car (car L))
   )))))



;this takes in the args and car/appends them where they
;/if they show up on the list then add a1 or a2 if they show.
(define (Substitute a1 a2 L)
       (cond
         ((null? L) '())
         ((list? (car L))
          (cons (Substitute a1 a2 (car L))(Substitute a1 a2 (cdr L))))
         ((eq? (car L) a1)(cons a2 (Substitute a1 a2 (cdr L)))) 
         (else
          (cons (car L)
                (Substitute a1 a2 (cdr L)))))) 
       
  

(Reflexive? '((a a) (b b)) '(a b))
(Symmetric? '((a b) (b a) (c d) (d c)))
(Transitive? '((a b) (b c) (a c)))
(Substitute 'bim 'bam '(a b bim(c d bim e)(f (g h(i bim)) j)))


