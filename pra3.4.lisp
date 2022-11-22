



;pra3.4
;make a account
(define (make-account balance pwd)
  (let ((count 0))

    (define (tip x)
      (set! count (+ count 1))
      (if (< count 7)
        "incorrest password"
        (call-the-cops)
        )
      )


    (define call-the-cops
      (lambda() (display "!")))

    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        (error "insuffcient amount")
        )
      )

    (define (deposit amount)
      (begin (set! balance (+ balance amount)) balance)
      )

    (define (dispatch p m)
      (cond
        ((and (eq? p pwd)(eq? m 'withdraw)) withdraw)
        ((and (eq? p pwd)(eq? m 'deposit)) deposit)
        ((and (not (eq? p pwd))
              (or (eq? m 'withdraw) (eq? m 'deposit)))
         tip
         )
        (else (error "unknown request "))
        )
      )

    dispatch)
  )

(define s (make-account 100 'hello))
