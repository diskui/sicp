(define (fr n)
    (if(< n 3) n
        (+ (fr (- n 1))
        (* 2 (fr (- n 2)))
        (* 3 (fr (- n 3) ))
           )
    )
  )


(define (fi n)
  (define (f a b c counter)
    (cond
    ((< n 3)n)
    ((<= counter 0)a)
    (else(f(+ a (* 2 b) (* 3 c)) a b (- counter 1)))
    )
  )
  (f 2 1 0 (- n 2))
)
