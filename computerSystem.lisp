
;square
(define (square x) (* x x))


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


;构造复数
;rectangular
(define (make-from-real-imag-rectangular x y)(attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)(attach-tag 'rectangular (cons (* (cos a) r)(* (sin a) r))))
(define (real-part-rectangular x)(car x))
(define (imag-part-rectangular x)(cdr x))
(define (magnitude-rectangular x)(sqrt (+ (square (real-part-rectangular x))(square (imag-part-rectangular x)))))
(define (angle-rectangular x)(atan (imag-part-rectangular x) (real-part-rectangular x)))

;magnitude
(define (make-from-mag-ang-polar r a)(attach-type 'polar (cons r a))
  (define (make-from-real-imag-polar x y)(attach-tag 'polar (cons (sqrt (+ (square x)(square y)))(atan y x))))
  (define (magnitude-polar x)(car x))
  (define (angle-polar x)(cdr x))
  (define (real-part-polar x)(* (magnitude-polar)(cos (angle-polar x))))
  (define (imag-part-polar x)(* (magnitude-polar)(sin (angle-polar x))))
  )

;total
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y)
  )

(define (make-from-mag-ang x y)
  (make-from-mag-ang-polar x y)
  )

;类型判断
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular)
  )

(define (polar? z)
  (eq? (type-tag z) 'polar)
  )

;选择函数
(define (real-part z)
  (cond
    ((rectangular? z) (real-part-rectangular z))
    ((polar? z)(real-part-polar z))
    (else (error "unknown type -- real part" z))
    ))

(define (imag-part z)
  (cond
    ((rectangular? z) (imag-part-rectangular z))
    ((polar? z)(imag-part-polar z))
    (else (error "unknown type -- imag part" z))
    ))


(define (magnitude z)
  (cond
    ((rectangular? z) (magnitude-rectangular z))
    ((polar? z)(magnitude-polar z))
    (else (error "unknown type -- magnitude" z))
    ))

(define (angle z)
  (cond
    ((rectangular? z) (angle-rectangular z))
    ((polar? z)(angle-polar z))
    (else (error "unknown type -- angle" z))
    ))

;复数的运算
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))(+ (imag-part z1) (imag-part z2)))
  )

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))(- (imag-part z1) (imag-part z2)))
  )

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angel z1)(angle z2)))
  )

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angel z1)(angle z2)))
  )




















;有理数
;构造有理数

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (number x) (car x))

(define (denom x) (cdr x))

;打印有理数
(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x))
  )



;有理数运算

(define (add-rat a b)
  (make-rat (+(* (number a) (denom b))
              (* (denom a) (number b)))
            (* (denom a)(denom b))
            )
  )

(define (sub-rat a b)
  (make-rat(- (* (number a)(denom b))(* (number b)(denom a)))
    (* (denom a)(denom b))
    )
  )

(define (mul-rat)
  (make-rat (* (number a) (number b))(* (denom a)(denom b)))
  )

(define (div-rat a b)
  (make-rat (* (number a)(denom b))(* (number b) (denom a)))
  )

(define (equal-rat? a b)
  (= (* (number a) (denom b))(* (nubmer b)(denom a)))
  )

;最大公约数
(define (gcd a b)
  (if (= b 0) a
    (gcd b (remainder a b))
    )
  )

