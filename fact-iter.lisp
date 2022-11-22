(define (fact n)
    (define(fact-iter counter product n)
        (if(> counter n)product
            (fact-iter (+ 1 counter)(* counter product) n)
        )
    )
    (fact-iter 1 1 n)
    
  )



