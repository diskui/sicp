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

(define (sqrt x)
    (fix-point (lambda (y) (average y (/ x y))) 1.0)
  )

(define (average x y)
    (/ (+ x y) 2)
  )
