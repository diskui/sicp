(define (fix-point-of-transform g transform guess)
    (fix-point (transform g) guess)
  )

(define (average a b)
    (/ (+ a b) 2)
  )
(define (average-damp f)
    (lambda (x) (average (f x) x))
  )
(define (newton-transform f)
    (lambda (x)(- x (/ (f x) ((deriv f) x))))
  )

(define (deriv f)
   (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)) 

  )

(define dx 0.001)

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

(define (square x) (* x x))

(define (sqrt1 x)
    (fix-point-of-transform (lambda (y) (/ x y)) average-damp 1.0)
  )

(define (sqrt2 x)
    (fix-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0)
  )

(define (cube-root x)
    (fix-point (lambda (y)(/ (+ (/ x (square y)) y) 2)) 1.0)
  )
