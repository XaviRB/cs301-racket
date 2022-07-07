#lang racket
;Xavier Rodriguez
;CS301
;LAB7

;HELPERS
(define (opps L)
  (filter (lambda (List)
    (and (not (equal? List 'and))
         (not (equal? List 'or))
         (not (equal? List 'not))
         (not (equal? List 'implies))
         (not (equal? List 'iff))
         ))
 
  (flatten L)
  ))


(define (find L)
  (cond
    ((eq? (car (cdr L)) 'and)     (car (cdr L)))
    ((eq? (car (cdr L)) 'or)      (car (cdr L)))
    ((eq? (car (cdr L)) 'implies) (car (cdr L)))
    ((eq? (car (cdr L)) 'not)     (car (cdr L)))
    ((eq? (car (cdr L)) 'iff)     (car (cdr L)))
    (else #f))
  )

;FUNCTIONS

;What this function does it removes and looks for the terms
;and or not implies and iff.
(define (collect-prop-variables L)
  (cond  
   ((empty? L) L)
  
   (else
    (let ((i (opps L)))
    (remove-duplicates i))))
)

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

;what this function is supposed to do is the
;opperations. And what i wanted to do was write a case
;for each opperator it hit and if it hits it
;to hold that boolean either t or f and
;then call it recursivly so it can go through the
;whole list but i ran out of time 
(define (Evaluate-WFF L)
 (define cleanL(flatten L))
 (if (equal? (car cleanL) #t) 
       (cond
         ((equal? (find cleanL) 'or) #t)
         ((equal? (find cleanL) 'and)#t)
         ((equal? (find cleanL) 'not)#t)
         ((equal? (find cleanL) 'implies)#t)
         ((equal? (find cleanL) 'iff)#t)
        )
       #f)
  )



;tests
;(collect-prop-variables '(A and (not A)))
(collect-prop-variables '(B iff (A or C)))

;(Substitute '(C or (D or D)) 'D #f)
;(Substitute '(not ((B and A) or (A implies B))) 'B '(#t and C) )

;(Evaluate-WFF '(#t or (not #t)))
