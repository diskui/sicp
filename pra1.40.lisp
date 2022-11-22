(define (newton-transform f)
    (lambda (x)(- x (/ (f x) ((deriv f) x))))
  )

(define tolerance 0.00001)

(define (fix-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
      )
    (define (try guess)
        (let ((next (f guess)))
        (if(close-enough? guess next)
            next
            (try next)
          )
          )
      )
    (try first-guess)
  )


(define (newton-method f guess)
    (fix-point (newton-transform f) guess)
  )

(define (cubic a b c)
    (lambda (x) (+ (* x x x) (* a (* x x)) (* b x) c))
  )
