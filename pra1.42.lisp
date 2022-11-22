(define (compose f g)
    (lambda (x) (f (g x)))
  )

(define (repeated f n)
    (cond ((= n 1) (lambda (x) (f x)))
    (else (compose f (repeated f (- n 1)))
      )
  ))




(define (inc x) (+ x 1))

(define (square x) (* x x))
