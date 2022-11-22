
(define (raise x)(apply-generic 'raise x))

(put 'raise 'integer (lambda (x)(make-rational x 1)))

(put 'raise 'rational (lambda (x)(make-real (/ (number x)(denom x)))))

(put 'raise 'real (lambda (x)(make-from-real-imag x 0)))


;pra2.84
(define (apply-generic op . args)

  (define (raise-into s t)
    (let ((s-type (type-tag s))
          (t-type (type-tag t)))
      (cond
        ((equal? s-type t-type) s)
        ((get 'raise (list s-type))
         (raise-into ((get 'raise (list s-type)) (contents s)) t))
        (else #f)
        )
      ))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((a1 (car args))
                (a2 (cadr args)))
            (cond
              ((raise-into a1 a2)(apply-generic op (raise-into a1 a2) a2))
              ((raise-into a2 a1)(apply-generic op a1 (raise-into a2 a1)))
              (else (error "no method for these types" (list op type-tags)))
              ))
          (error "no method for these types " (list op type-tags)))
        )))
  )

