(define (double x)
    (+ x x)
  )

(define (halve x)
    (/ x 2)
  )

(define (even? x)
    (= (remainder x 2) 0)
  )

(define (multiply x y)
    (cond ((= y 0) 0)
    ((even? y)(double (multiply x (halve y))))
    (else (+ x (multiply x (- y 1))))
          )

  )

(define (fast-multiply x y)
    (define (helper x y product)
        (cond((= y 0)product)
        ((even? y)(helper (double x)(halve y) product))
        (else(helper x (- y 1) (+ x product)))
          )
      )
    (helper x y 0)
  )
