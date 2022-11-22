;recursive
(define (accumulate combiner null-value term a next b)
    (if(> a b) null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))
               )
  )
;iterative
(define (accumulate-iter combiner null-value term a next b)
    (define (iter a res)
        (if(> a b) res
            (iter (next a) (combiner res (term a)))
                   )
      )
    (iter a null-value)
  )

(define (identity n) n)
(define (inc n) (+ n 1))


(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 identity a inc b))


(define (sum-iter term a next b) (accumulate-iter + 0 term a next b))
(define (product-iter term a next b) (accumulate-iter * 1 identity a inc b))
