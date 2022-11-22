
(define nil '())

;reverse颠倒表的顺序
;(define (reverse x)
;    (define (iter items result)
;        (if (null? items)
;            result
;            (iter (cdr items) (cons (car items) result))
;          )
;      )
;    (iter x nil)
;  )



;fold-right
(define (fold-right op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (fold-right op initial (cdr sequence))
            )
      )
   )

;fold-left
(define (fold-left op initial sequence)
  (define (iter result rest)
        (if (null? rest)
          result
          (iter (op result (car rest)) 
                (cdr rest)))
    )
  (iter initial sequence)
   ) 

(define (reverse-right sequence)
    (fold-right (lambda(x y) (append y (list x))) nil sequence)
  )

(define (reverse-left sequence)
    (fold-left (lambda(x y) (cons y x)) nil sequence)
  )
