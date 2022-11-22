



;pra3.3
;make a account
(define (make-account balance pwd)
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
      ((and (not (eq? p pwd)) (eq? m 'withdraw)) (lambda (x) "incorrect password"))
      ((and (not (eq? p pwd)) (eq? m 'deposit)) (lambda (x) "incorrect password"))
      (else (error "unknown request "))
      )
    )

  dispatch)


(define s (make-account 100 'hello))
