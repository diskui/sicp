(define (sum term a next b)
    (if(> a b) 0
        (+ (term a)
           (sum term (next a) next b)
           ))
  )

(define (inc n) (+ n 1))

(define (sum-cube a b)
    (sum cube a inc b)
  )

(define (cube n) (* n n n))

(define (identity n) n)

(define (sum-integer a b)
    (sum identity a inc b)
  )

(define (pi-term x)
    (/ 1.0 (* x (+ x 2)))
  )

(define (pi-next x)
    (+ x 4)
  )

(define (pi-sum a b)
    (sum pi-term a pi-next b)
  )


(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2)) add-dx b)
       dx)
  )

(define (simpson f a b n)


  )

