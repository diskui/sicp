
;varialbe?
(define (variable? x)
    (symbol? x)
  )


;same-variable?
(define (same-varialbe? x y)
    (and (variable? x) (variable? y) (eq? x y))
  )

;=number?
(define (=number? exp n)
    (and (number? exp) (= exp n))
  )

;make sum list
(define (make-sum-list l)
    (if(= (length l) 2)
        (list '+ (car l) (cadr l))
        (make-sum (car l) (make-sum-list (cdr l)))
      )
  )


;make sum
(define (make-sum x y)
  (cond
        ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y))(+ x y))
        (else (make-sum-list (list x y)))
    )
  )

;make product list
(define (make-product-list l)
    (if(= (length l) 2)
        (list '* (car l) (cadr l))
        (make-product (car l) (make-product-list (cdr l)))
      )
  )

;make product
(define (make-product x y)
  (cond
        ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y) 
        ((=number? y 1) x) 
        ((and (number? x) (number? y)) (* x y))
        (else (make-product-list (list x y)))
    )
  )

;sum?
(define (sum? x)(and (pair? x) (eq? (car x) '+)))

;addend
(define (addend x) (cadr x))

;augend
(define (augend x) 
  (let(
    (a (cddr x)))
    (if (= (length a) 1) 
      (car a)
      (make-sum-list a)
      )
    )
  )

;product?
(define (product? x) (and (pair? x) (eq? (car x) '*)))

;multiplier
(define (multiplier x)
  (let(
    (m (cddr x)))
    (if(= (length m) 1) 
      (car m)
      (make-product-list m)
      )
    ))

;multiplicand
(define (multiplicand x) (caddr x))

;exponentiation?
(define (exponentiation? exp)(and (pair? exp) (eq? (car exp) '**)))

;base
(define (base x) (cadr x))

;exponent
(define (exponent x) (caddr x))

(define (make-exponentiation base exponent)
  (cond
        ((=number? base 1) 1)
        ((=number? base 0) 0)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))
    )
  )

;memq
(define (memq item x)
    (cond
         ((null? x) #f)
         ((eq? (car x) item) x)
         (else (memp item (cdr x)))
      )
  )

;is two exp equal?
(define (equal? x y)
    (cond
      ((and (not (pair? x))(not (pair? y))) (eq? x y))
      ((and (pair? x) (pair? y)) (and (equal? (car x)(car y)) (equal? (cdr x)(cdr y))))
      (else #f)
      )
  )

;deriv of symbol exp
(define (deriv exp var)
    (cond
        ((number? exp) 0)
        ((variable? exp)(if (same-varialbe? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp) (deriv (multiplier exp) var))))
        ((and (exponentiation? exp) (= 0 (deriv (exponent exp) var)))
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (if(number? (exponent exp))
                                                            (- (exponent exp) 1)
                                                            ('(- (exponent exp) 1)))))
                       (deriv (base exp) var))
         )
        (else (error "unknown expression type -- DERIV" exp))
      )
  )

