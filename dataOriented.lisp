

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


;square
(define (square x) (* x x))


;install rectangular package
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

;apply generic
(define (apply-generic op . arg)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types -- apply generic" (list op type-tags))
        )
      )
    )
  )

;选择函数
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;构造函数
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y)
  )
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a)
  )





