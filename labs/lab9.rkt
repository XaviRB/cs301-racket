#lang racket
;Xavier Rodriguez
;CS301
;LAB9

;Helper functions
;This function takes in the expersion
;and checks recursivly that each part in the
;equation follows the grammer 
(define (expersion? A)
   (cond
     ((null? A) A)
       
     ((member (car A) '(#\( ))
      (expersion? (cdr A)))
     
     ((member (car A) '( #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\)))
      (expersion? (cdr A)))
     
     ((member (car A) '(#\+ #\*))
      (expersion? (cdr A)))
     
     (else (car A))
))


;main function that calls the string and
;makes it a list and covers if something that
;is not supposed to be in the expersion 
(define (parse List)
  (let ((L (string->list List)))
    (begin
      (if (eq? (car L) #\( )
          #t
          (error "Error: no open parentheses"))
      (if (eq? (expersion? L) '())
          #t
          (error "Error: Was Expecting a Number or Operator But Was Given" (expersion? L) ))
      (if (eq? (last L) #\) )
          #t
          (error "Error: no open parentheses"))
    )
  )     
)

(parse "((1+2)*(5+5))")