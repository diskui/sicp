

;rational package
(put 'project 'rational (lambda(x)(make-scheme-number (round (/ (number x)(denom x))))))

;real package

;complex package
(put 'project 'complex
     (lambda (x)(make-real (real-part x))))

;drop x
(define (drop x)
  (let ((project-proc (get 'project (type-tag x))))
    (if project-proc
      (let ((project-number (project-proc (contents x))))
        (if (equ? project-number (raise-project-number))
          (drop project-number)
          x))
      x))
  )
