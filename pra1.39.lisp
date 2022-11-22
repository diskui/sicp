(define (cont-frac-recu n d k)
    (if (= k 0) 0
    (/ (n k) (+ d k) (cont-frac n d (- k 1))) 
      )
  )

(define (cont-frac-iter n d k)
    (define (iter term res)
      (if (= term 0) res
        (iter (- term 1) (/ (n term)
                         (+ (d term) res)))))
    (iter k 0)
  )



(define (tangent x k)
    (define (n i)
        (if (= i 1) x
          (- (* x x)))
      )
    (define (d i)
        (- (* 2 i) 1)
      )
    (cont-frac-iter n d k)
  )


 (define (tan-cf x k) 
   (cont-frac-iter (lambda (i) 
                (if (= i 1) x (- (* x x)))) 
              (lambda (i) 
                (- (* i 2) 1)) 
              k))
