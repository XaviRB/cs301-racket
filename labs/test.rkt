#lang racket
;removes the first single element put in

(define (expersion? A)
   (cond
     ;((eq? A '() ) A)
     ((null? A) A)

       
     ((member (car A) '(#\( ))
      (expersion? (cdr A)))
     
     ((member (car A) '( #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\)))
      (expersion? (cdr A)))
     
     ((member (car A) '(#\+ #\*))
      (expersion? (cdr A)))
     
     (else (car A))
))




(define (parse List)
  (let ((L (string->list List)))
    (begin
      (if (eq? (car L) #\( ) #t
          (error "no open parentheses"))
      (expersion? L) 
    
    )
  )     
)

(parse "((1+2)*(5+))")