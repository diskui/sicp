(define (cube x)
   (define (cube-iter guess)
     (if (goodenough? guess)
        guess
        (cube-iter (improve guess))
       )
     ) 
   (define (goodenough? guess)
     (= (improve guess) guess)
     )
   (define (cubic guess)
     (* guess guess guess)
     )
   (define (improve guess)
     (/ (+ (/ x (* guess guess)) (* guess 2)) 3)
     )
   (cube-iter 1.0)
  )
