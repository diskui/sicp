(define nil '())

(define (fringe items)
  (define (iter t r)
        (cond 
          ((null? t) r)
          ((not (pair? t)) (cons t r)) 
          (else (iter (car t) (iter (cdr t) r)))
          )
    )
  (iter items nil)
  )


(define (fringe-recu tree) 
   (if (null? tree)  
       nil 
         (if (not (pair? (car tree))) 
             (cons (car tree) (fringe-recu (cdr tree))) 
             (cons (fringe-recu (car tree)) (fringe-recu (cdr tree))))))
