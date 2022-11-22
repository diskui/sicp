

;negate
(define (negate x)(apply-generic 'negate x))

;scheme number
(put 'negate 'scheme-number (lambda (x)(tag (- x))))


;rational number
(put 'negate 'rational (lambda (x) (make-rational (- (number x)) (denom x))))


;complex number
(put 'negate 'complex (lambda (z)
                        (make-from-real-imag (negate (real-part z))
                                             (negate (imag-part z)))))

;polynomial
(define (negate-terms termlist)
  (if (empty-termlist? termlist)
    (the-empty-termlist)
    (let ((t (first-term termlist)))
      (adjoin-term (make-term (order t)(negate (coeff t)))
                   (negate-terms (rest-terms termlist)))))
  )

(put 'negate 'polynomial (lambda(t)
                           (make-termlist (variable t)
                                          (negate-terms (term-list t)))))

(put 'sub '(polynomial polynomial) (lambda (x y)
                                     (tag (add-poly x (negate y)))))

