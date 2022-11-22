
;组合类型与内容
;attach type-tag and contents
(define (attach-tag type-tag contents)
  (cons type-tag contents)
  )

;get the type-tag of a datum
(define (type-tag datum)
  (if(pair? datum)
    (car datum)
    (error "bad tagged datum -- TYPE-TAG" datum))
  )

;get the content of a datum
(define (contetns datum)
  (if (pair? datum)
    (cdr datum)
    (error "bad tagged datum -- CONTENTS" datum))
  )


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



;deriv dispatch
(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((varible? exp) (if (same-varible? exp var) 1 0))
    (else ((get 'deriv (operator exp)) (operands exp) var))
    )
  )



;install sum deriv package
(define (install-sum-package)
  (define (make-sum x y) (cons x y))
  (define (addend z) (cadr z))
  (define (audend z) (caddr z))
  (define (diriv-sum x) (make-sum (deriv (addend x)) (deriv (audend x))))

  (define (tag x) (attach '+ x))
  (put 'deriv '+ deriv-sum)
  (put 'make-sum '+ (lambda(x y) (tag (make-sum x y))))
  'done
  )

(define (make-sum x y) ((get 'make-sum '+) x y))

;install product deriv package
(define (install-product-package)
  (define (make-product x y) (cons x y))
  (define (multiplier p) (cadr p))
  (define (multiplicand p)(caddr p))
  (define (deriv-product x)
    (make-sum
      (make-product (multiplier p)(deriv-product (multiplicand p) var))
      (make-product (multiplicant p)(deriv-product (multiplier p) var))))

  (define (tag x) (attach-tag '* x))
  (put 'deriv '* deriv-product)
  (put 'make-product '* (lambda (x y) (tag (make-product x y))))
  'done
  )

(define (make-product x y)
  ((get 'make-product '*) x y)
  )

