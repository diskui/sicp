;map
(define (mymap proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (mymap proc (cdr items)))
      )
  )

;accumulate
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
            )
      )
  )

;叶子计数
;(define (count-leaves x)
;    (cond ((null? x) 0)
;    ((not (pair? x)) 1)
;    (else (+ (count-leaves (car x))
;             (count-leaves (cdr x))))
;          )
;  )

(define (count-leaves tree)
    (accumulate + 0 (mymap (lambda(node) (if(pair? node) (count-leaves node) 1)) tree))
  )

