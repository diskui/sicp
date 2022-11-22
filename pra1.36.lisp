(define tolerance 0.00001)

(define (fix-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
      )
    (define (try guess)
        (display guess)
        (newline)
        (let ((next (f guess)))
        ;(newline)
        ;(display next)
        (if(close-enough? guess next)
            next
            (try next)
          )
          )
      )
    (try first-guess)
  )

