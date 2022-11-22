

;apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2-> t1 (get-coercion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                    (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types" (list op type-tags))))))
          (error "No method for these types" (list op type-tags))
          ))
      ))
  )




;install rectangular package
;安装直角坐标系包
(define (install-rectangular-package)
  ;internal procedures
  (define (real-part z)(car z))
  (define (imag-part z)(cdr z))
  (define (make-from-real-imag x y)(cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (atan (imag-part z)(real-part z)))
  (define (make-from-mag-ang r a) (cons (* r (cos a))(* r (sin a))))

  ;interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'mugnitude '(rectangular) mugnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda(x y)(tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda(x y)(tag (make-from-mag-ang r a))))
  'done
  )


;install polar package
;安装极坐标系包
(define (install-polar-package)
  ;internal procedures
  (define (real-part z)(* (magnitude z) (cos (angle z))))
  (define (imag-part z)(* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)(cons (sqrt (+ (square x)(square y)))(atan y x)))
  (define (magnitude z)(car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))

  ;interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'mugnitude '(polar) mugnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda(x y)(tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda(x y)(tag (make-from-mag-ang r a))))
  'done
  )


;gcd
(define (gcd a b)
  (cond
    ((= b 0) a)
    ((= a 0) b)
    (else (gcd b (remainder a b)))
    )
  )


;组合类型与内容
;attach type-tag and contents
;(define (attach-tag type-tag contents)
;  (cons type-tag contents)
;  )
;
;;get the type-tag of a datum
;(define (type-tag datum)
;  (if(pair? datum)
;    (car datum)
;    (error "bad tagged datum -- TYPE-TAG" datum))
;  )
;
;;get the content of a datum
;(define (contetns datum)
;  (if (pair? datum)
;    (cdr datum)
;    (error "bad tagged datum -- CONTENTS" datum))
;  )


;pra2.78
(define (attach-tag type-tag contents)
  (if(number? contents)
    contents
    (cons(type-tag contents)))
  )

(define (type-tag datum)
  (cond
    ((number? datum) 'scheme-nubmer)
    ((pair? datum) (car datum))
    (else (error "wrong datum -- type-tag" datum))
    )
  )

(define (contents datum)
  (cond
    ((number? datum) datum)
    ((pair? datum) (cdr datum))
    (else (error "wrong datum -- contents" datum))
    )
  )


;define the basic operators
(define (add x y)(apply-generic 'add x y))
(define (sub x y)(apply-generic 'sub x y))
(define (mul x y)(apply-generic 'mul x y))
(define (div x y)(apply-generic 'div x y))

;pra2.79
(define (equ? a b)(apply-generic 'equ? a b))

;pra2.80
(define (=zero? n)(apply-generic '=zero n))

;install basic number
;安装基本数
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)(labmda (x y)(tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)(labmda (x y)(tag (- x y))))
  (put 'mul '(scheme-number scheme-number)(labmda (x y)(tag (* x y))))
  (put 'div '(scheme-number scheme-number)(labmda (x y)(tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))

  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero 'scheme-number (lambda(x)(= x 0)))
  'done
  )

;make a scheme number
(define (make-scheme-number n) ((get 'make 'scheme-number) n))

;install rational number
;有理数
(define (install-rational-number)
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g)(/ d g))
      )
    )
  (define (number x)(car x))
  (define (denom x)(cdr x))
  (define (add-rat x y)
    (make-rat (+ (* (number x)(denom y))(+ (number y) (denom x)))
              (* (denom x)(denom y)))
    )

  (define (sub-rat x y)
    (make-rat (- (* (number x)(denom y)) (* (number y)(denom x)))
              (* (denom x)(denom y)))
    )

  (define (mul-rat x y)
    (make-rat (* (number x)(number y))
              (* (denom x)(denom y)))
    )

  (define (div-rat x y)
    (make-rat (* (number x)(denom y))
              (* (number y)(denom x)))
    )

  (define (equ? a b)(= (* (number a)(denom b)) (* (number b)(denom a))))
  (define (=zero? n)(= (number n) 0))

  ;interface with the rest of the system
  (define (tag x)(attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))

  (put 'equ? '(rational rational) equ?)
  (put '=zero 'rational =zero?)
  'done
  )

(define (make-rational n d)((get 'make 'rational) n d))

;install the complex package
;复数
(define (install-complex-package)
  ;(install-rectangular-package)
  ;(install-polar-package)
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex x y)
    (make-from-real-imag
      (+ (real-part x)(real-part y))
      (+ (imag-part x)(imag-part y))))
  (define (sub-complex x y)
    (make-from-real-imag
      (- (real-part x)(real-part y))
      (- (imag-part x)(imag-part y))))
  (define (mul-complex x y)
    (make-from-mag-ang
      (* (magnitude x)(magnitude y))
      (+ (angle x)(angle y))))
  (define (div-complex x y)
    (make-from-amg-ang
      (/ (magnitude x)(magnitude y))
      (- (angle x)(angle y)))
    )

  (define (=zero? n)(= (real-part n) (imag-part n) 0))

  (define (equ? a b)(and (= (real-part a)(real-part b))(= (imag-part a)(imag-part b))))


  ;interfact with numbers of different types
  (define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))

  ;interface to the rest of the system
  (define (tag x)(attach-tag 'complex x))
  (put 'add '(complex complex)(lambda x y)(tag (add-complex x y)))
  (put 'sub '(complex complex)(lambda x y)(tag (sub-complex x y)))
  (put 'mul '(complex complex)(lambda x y)(tag (mul-complex x y)))
  (put 'div '(complex complex)(lambda x y)(tag (div-complex x y)))
  (put 'make-from-real-imag 'complex (lambda (x y)(tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a)(tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'mugnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'equ? '(rational rational) equ?)
  (put '=zero? 'rational =zero?)

  (put 'add '(complex scheme-number) (lambda (x y)(tag (add-complex-to-schemenum x y))))
  'done
  )

(define (make-complex-from-real-imag x y)((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)((get 'make-from-mag-ang 'complex) r a))




