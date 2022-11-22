

;monte carlo
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0) (/ trials-passed trials))
      ((experiment) (iter (- trials-remaining 1) (+ trials-padded 1)))
      (else (iter (- trials-remaining 1) (trials-passed)))
      )
    )
  (iter trials 0)
  )

;get a random number in a range
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))
    )
  )

(define (P x y)

  )


;estimate integral
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (exp)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (monte-carlo trials exp)
  )


