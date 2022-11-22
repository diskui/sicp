

(define (sine x)(apply-generic 'sine x))
(define (cosine x)(apply-generic 'cosine x))
(define (arctan x)(apply-generic 'arctan x))
(define (exp x y)(apply-generic 'exp x y))


;add into rational number package
(put 'sine '(number) (lambda (x)(tag (sin x))))
(put 'cosine '(number) (lambda (x)(tag (cosin x))))
(put 'arctan '(number) (lambda (x)(tag (arctan x))))
(put 'exp '(number number) (lambda (x y)(tag (expt x y))))

;complex rectangular package
(define (square x)(mul x x))
(define (sqrt x) (exp x 0.5))
(define (make-from-mag-ang r a)(cons (mul r (cosine a))(mul r (sine a))))
(define (magnitude x)(sqrt (add (square (real-part x)(square (imag-part x))))))
(define (angle x)(arctan (div (imag-part x)(real-part x))))

;complex polar package
(define (real-part x)(mul (magnitude x)(cosine (angle x))))
(define (imag-part x)(mul (magnitude x)(sine (angle x))))


;complex package
(define (add-complex x y)
  (make-from-real-imag (add (real-part x)(real-part y))
                       (add (imag-part x)(imag-part y))))
(define (sub-complex x y)
  (make-from-real-imag (sub (real-part x)(real-part y))
                       (sub (imag-part x)(imag-part y))))

(define (mul-complex x y)
  (make-from-mag-ang (mul (magnitude x)(magnitude y))
                     (add (angle x)(angle y)))
  )

(define (div-complex x y)
  (make-from-mag-ang (div (magnitude x)(magnitude y))
                     (sub (angle x)(angle y)))
  )
