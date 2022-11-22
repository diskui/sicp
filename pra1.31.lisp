;递归
(define (product term a next b)
    (if (> a b) 1
        (* (term a)
           (product term (next a) next b)
           )
      )
  )
;迭代
(define (product-iter term a next b)
  (define (iter a res)
    (if (> a b) res
        (iter (next a) (* res (term a)))
      )
    )
 (iter a 1)   
  )




(define (even? n)
    (= 0 (remainder n 2))
  )

(define (pi-term n)
   (if(even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))
     )
  )


(define (identity n) n)

(define (inc n) (+ n 1))

(define (factorial n)
    (product identity 1 inc n)
  )

