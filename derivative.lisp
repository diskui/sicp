(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))

  )

(define dx 0.001)

(define (cube x) (* x x x))
