(define (iterative-improve good-enough? improve)
    (lambda (guess)
        (if (good-enough? guess) guess
        (iterative-improve good-enough? (improve guess))
          )
      )
  )

(define (close-enough? a b)
    (< (abs (- a b)) 0.001)
  )

(define (square x) (* x x))

(define (sqrt x)
    ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001)) (lambda (guess)(/ (+ guess (/ x guess)) 2))) 1.0)

  )

(define (fix-point f)
    ((iterative-improve (lambda(guess)(close-enough? guess (f guess))) f) first-guess)

  )

