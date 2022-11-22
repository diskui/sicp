(define (make-mobile left right)
    (list left right)
  )

(define (make-branch length structure)
    (list length structure)
  )

;a
(define (left-branch x)
    (car x)
  )

(define (right-branch x)
    (cadr x)
  )

(define (branch-length x)
    (car x)
  )

(define (branch-structure x)
    (cadr x))

(define (total-weight x)
    (cond ((null? x) 0) 
    ((not(pair? x)) x)
    (else  (+ (total-weight (branch-structure (left-branch x)))
         (total-weight (branch-structure (right-branch x)))
      ))
    )
 )

(define (torque branch)
       (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balance? mobile)
  (if (not (pair? mobile)) 
    #t
    (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
         (balance? (branch-structure (left-branch mobile)))
         (balance? (branch-structure (right-branch mobile)))
         )
    )
  )

