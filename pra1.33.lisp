(define (gcd a b)
    (if (= b 0) a
    (gcd b (remainder a b))
      )
  )

(define (identity n) n)


(define (prime? n)
    (if (= n 1) #f 
    (= (smallest-divisor n) n))
  )

(define (smallest-divisor n)
    (find-divisor n 2)
  )

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divide? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))
      )
  )
(define (divide? a b)
    (= (remainder b a) 0)
  )
(define (square n)
    (* n n)
  )

(define (inc n) (+ n 1))

;recursive
(define (accumulate combiner null-value term a next b filter)
    (if(> a b) null-value
        (combiner (if (filter a) (term a) null-value)
                  (accumulate combiner null-value term (next a) next b filter))
               )
  )
;iterative
(define (accumulate-iter combiner null-value term a next b)
    (define (iter a res)
        (cond((> a b) res)
            ((filter a)(iter (next a) (combiner res (term a))))
            (else (iter (next a) res))
                   )
      )
    (iter a null-value)
  )

(define (sum-of-prime-squares a b)
    (accumulate + 0 square a inc b prime?)
  )

(define (product-of-relative-primes n)
  (define (gcd? a)
        (= (gcd a n) 1)
          )
    (accumulate * 1 identity 1 inc n gcd?)
  )

