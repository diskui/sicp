



;square
(define (square x) (* x x))


;make a complex with real and imagine parts
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude (sqrt (+ (square x) (square y)))))
      ((eq? op 'angle) (atan y x))
      (else (error "unknown op -- make-from-real-imag" op))
      )
    )
  dispatch
  )

;apply generic
(define (apply-generic op arg) (arg op))

;make a complex with magitude and angle
(define (make-from-mag-ang r a)
  (define (dispacth op)
    (cond
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      ((eq? op 'real-part)(* r (cos a)))
      ((eq? op 'imag-part)(* r (sin a)))
      (else (error "unknown op --make-from-mag-ang" op))
      )
    )
  )
