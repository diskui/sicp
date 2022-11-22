(define (sqrt x)

    (define (sqrt-iter guess)
        (if(goodenough? guess)
             guess
            (sqrt-iter (improve guess) 
            )
        )
    )

    (define (goodenough? guess)
        (< (abs(-(improve guess)guess)) 0.001) 
    )

    (define (improve guess)
        (average (/ x guess) guess) 
    )

    (define (square x)
        (* x x)
    )

    (define (average a b)
        (/ (+ a b) 2)
    )

    (sqrt-iter 1.0)
)



