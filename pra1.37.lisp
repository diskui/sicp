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


(define (n i) 1)

(define (d i)
    (cond
    ((= i 1) 1)
    ((= (remainder i 3) 2)(* 2 (/ (+ i 1) 3)))
    (else 1)
      )
  )
