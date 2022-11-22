





;accumulator
(define (make-accumulator init)
  (define (inter n)
    (begin (set! init (+ n init)) init)
    )
  inter
  )
