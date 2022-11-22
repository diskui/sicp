

;same-variable?
(define (same-varialbe? x y)
    (and (variable? x) (variable? y) (eq? x y))
  )


;nil
(define nil '())

;singleton?
(define (singleton? x)
   (= (length x) 1) 
  )

;varialbe?
(define (variable? x)
    (symbol? x)
  )


;=number?
(define (=number? exp n)
    (and (number? exp) (= exp n))
  )

;memq
(define (memq item x)
    (cond
         ((null? x) #f)
         ((eq? (car x) item) x)
         (else (memq item (cdr x)))
      )
  )



;accumulate
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
            )
      )
  )



;sum?
(define (sum? expr)
    (eq? '+ (smallest-op expr))
  )

;product?
(define (product? expr)
    (eq? '* (smallest-op expr))
  )

;smallest op 
(define (smallest-op expr)
    (accumulate (lambda (a b)
                    (if(operator? b)
                      (min-precedence a b)
                      a))
                'maxop
                expr
                )
  )

;precedence-table
(define *precedence-table*
  '((maxop . 10000)
    (minop . -10000)
    (+ . 0)
    (* . 1)
    ))


;operator?
(define (operator? x)
    (define (loop op-pair)
        (cond
            ((null? op-pair) #f)
            ((eq? x (caar op-pair)) #t)
            (else (loop (cdr op-pair)))
          )
      )
    (loop *precedence-table*)
  )

;min-precedence
(define (min-precedence a b)
    (if (precedence<? a b)
    a
    b)
  )

;precedence <?
(define (precedence<? a b)
    (< (precedence a) (precedence b))
  )

;precedence op
(define (precedence op)
  (define (loop op-pair)
    (cond
       ((null? op-pair) 10000) 
       ((eq? op (caar op-pair))(cdar op-pair))
       (else (loop (cdr op-pair)))
      )
    )
  (loop *precedence-table*)
  )


;augend expr
(define (augend expr)
    (let ((a (cdr (memq '+ expr))))
      (if (singleton? a)
        (car a)
        a)
      )
  )

;prefix 
(define (prefix sym list)
    (if(or(null? list)(eq? sym (car list)))
        nil 
        (cons (car list) (prefix sym (cdr list)))
      )
  )


;addend expr
(define (addend expr)
    (let ((a (prefix '+ expr)))
      (if (singleton? a)
        (car a)
        a
        )
      )
  )

;make a sum
(define (make-sum a1 a2)
    (cond
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number a2)) (+ a1 a2))
        (else (list a1 '+ a2))
      )
  )

;multiplier expr
(define (multiplier expr)
    (let ((m (prefix '* expr)))
      (if (sinpleton? m)
        (car m)
        m
        )
      )
  )

;multiplicand expr
(define (multiplicand expr)
    (let
        ((m (cdr (memq '* expr))))
        (if (singleton? m)
            (car m)
            m
          )
      )
  )

;make a product
(define (make-product m1 m2)
    (cond
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m1 0) (=number? m2 0)) 0)
        ((and (number? m1) (number? m2))(* m1 m2))
        (else (list m1 '* m2))
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


