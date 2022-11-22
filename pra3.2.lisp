

;square
(define (square x) (* x x))



;monitor
(define (make-monitored f)
  (let ((count 0))
    (define (inter m)
      (cond
        ((eq? m 'how-many-calls?) count)
        ((eq? m 'reset-count) (set! count 0) count)
        (else (set! count (+ count 1))
              (f m)
              )
        )
      )

    inter
    ))
