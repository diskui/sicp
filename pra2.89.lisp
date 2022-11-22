


(define (first-term termlist)
  (make-term (- (len termlist 1))(car termlist))
  )

(define (adjoin-term term term-list)
  (cond
    ((=zero? term) term-list)
    ((equ? (order term)(length term-list))(cons (coeff term)(term-list)))
    (else (adjoin-term term (cons 0 term-list)))
    )
  )
