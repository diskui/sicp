(define nil '())

(define x (list 1 2 3 4 5 6))

;表的映射
(define (mymap proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (mymap proc (cdr items)))
      )
    )

(define (subsets s)
    (if (null? s) (list nil)
      (let ((rest (subsets (cdr s))))
      (append rest (mymap (lambda (x)(cons (car s) x)) rest))
        ))
  )
