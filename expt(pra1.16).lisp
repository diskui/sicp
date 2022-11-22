(define (expt-recu b n)
    (if(= n 0) 1
        (* b (expt-recu b (- n 1)))
               )

  )

(define(expt-iter b n)
    (define (expt-it counter product)
        (if(= counter 0)product
            (expt-it (- counter 1)(* b product))
          )
            )
    (expt-it n 1)
  )

(define (fast-expt b n)
    (cond((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))
      )
  )

(define (fast-expt-iter b n)
    (define (fastExptIter n b a)
        (cond((= n 0) a)
        ((even? n) (fastExptIter (/ n 2) (square b) a))
        (else(fastExptIter (- n 1) b (* b a)))
                     )

      )
    (fastExptIter n b 1)


  )


(define (even? n)
    (= (remainder n 2) 0)
  )

(define (square x)
    (* x x)
  )

