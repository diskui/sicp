




(define test

  (let ((count 0))


    (define (add x)
      (set! count (+ count 1))
      (if (< count 2)
        "right"
        "wrong"
        )
      )
    add
    ))
