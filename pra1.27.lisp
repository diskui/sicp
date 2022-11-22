(define (square x)
    (* x x)
  )

(define (expmod base exp m)
    (cond ((= exp 0) 1)
    ((even? exp)
        (remainder (square (expmod base (/ exp 2) m)) m)
     )
    (else(remainder (* base (expmod base (- exp 1) m)) m))
          )
  )

(define (carmichael n)
  (define (iter n a)
    (if (= a n) #t
    (if (= (expmod a n n) a) (iter n (+ a 1)) #f)))
    
  (iter n 1)
  )

