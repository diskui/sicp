;递归
(define (sum term a next b)
    (if(> a b) 0
        (+ (term a)
           (sum term (next a) next b))))

;迭代
(define (sum-iter term a next b)
    (define (iter a result)
        (if(> a b)result
          (iter (next a) (+ (term a) result))
          )
      )
    (iter a 0)
  )

(define (inc n) (+ n 1))

(define (cube n) (* n n n))

(define (odd? n)
    (= 1 (remainder n 2))
  )


(define (simpson f a b n)
    (define h (/ (- b a) n))
    (define (y k)(f (+ a (* k h))))
    (define (simpson-term k)
        (* (cond((or(= k 0)(= k n)) 1)
             ((odd? k) 4)
             (else 2))
            (y k)
           ))
    (* (/ h 3)(sum-iter simpson-term 0 inc n))
  )

