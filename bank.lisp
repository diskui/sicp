



;make a account
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)) balance)
      (error "insuffcient amount")
      )
    )

  (define (deposit amount)
    (begin (set! balance (+ balance amount)) balance)
    )

  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "unknown request  -- dispatch" m))
      )
    )

  dispatch)
