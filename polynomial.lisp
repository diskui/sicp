

;add polynomials
(define (add-poly p1 p2)
  (if (same-varible (varible p1) (varible p2))
    (make-poly (varible p1)(add-terms (term-list p1)(term-list p2)))
    (error "Polys not in same var -- add poly" (list p1 p2))
    )
  )

;multiple polynimials
(define (mul-poly p1 p2)
  (if (same-varible (varible p1) (varible p2))
    (make-poly (varible p1)(mul-terms (term-list p1)(term-list p2)))
    (error "Polys not in same var -- mul poly" (list p1 p2))
    )
  )

;install the polynomial package
(define (install-polynomial-package)
  ;internal procedures
  (define (make-poly varible term-list)
    (cons varible term-list)
    )
  (define (varible p) (car p))
  (define (term-list p)(cdr p))
  ;same-varible? & varible?

  ;interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda(x y) (tag (add-poly x y))))
  (put 'mul '(polynomial polynomial) (lambda(x y)(tag (mul-poly x y))))
  (put 'make 'polynomial (lambda(var term) (tag (make-poly var term))))
  'done

  )


;add two termlists
(define (add-terms L1 L2)
  (cond
    ((empty-termlist? L1) L2)
    ((empty-termlist? L2) L1)
    (else
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (cond
          ((> (order t1) (order t2))(adjoin-term t1 (add-terms (rest-terms L1) L2)))
          ((> (order t2)(order t1))(adjoin-term t2 (add-terms (rest-terms L1) L2)))
          (else (adjoin-term (make-term (order t1)(add (coeff t1)(coeff t2)))
                             (add-terms (rest-terms L1) (rest-terms L2))))
          )
        )
      )
    )
  )


;multiply tow termlists
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2)))
  )


;multiply a termlist by all terms
(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L))
  (the-empty-termlist)
  (let ((t2 (first L)))
    (adjoin-term (make-term (+ (order t1)(order t2))
                            (mul (coeff t1)(coeff t2)))
                 (mul-term-by-all-terms t1 (rest-terms L)))
    )
  )

;adjoin term to a termlist
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list))
  )

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-term term-list) (cdr term-list))
(define (empty-termlist? termlist) (null? termlist))

(define (make-term order coeff)(list order coeff))
(define (order term)(car term))
(define (coeff term)(cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms)
  )

;pra2.87
;=zero?
(define (=zero? poly)
  (if (empty-termlist poly)
    (the-empty-termlist)
    (if (=zero? (coeff (first-term poly)))
      (=zero? (rest poly))
      #f))
  )



























