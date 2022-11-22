(define dx 0.00001)

(define (smooth f)
    (lambda (x) (average (f x)(f (+ x dx))(f (- x dx))))
  )

(define (n-fold-smooth f n)
    ((repeated smooth n) f)
  )

(define (average a b c)
    (/ 3 (+ a b c))
  )

(define (compose f g)
    (lambda (x) (f (g x)))
  )

(define (repeated f n)
    (cond ((= n 1) (lambda (x) (f x)))
    (else (compose f (repeated f (- n 1)))
      )
  ))
