(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y)))
  )

(define (mul-interval x y)
    (let(
        (p1 (* (lower-bound x)(lower-bound y)))
        (p2 (* (upper-bound x)(lower-bound y)))
        (p3 (* (upper-bound x)(upper-bound y)))
        (p4 (* (lower-bound x)(upper-bound y)))
         )
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))
      ))

(define (div-interval x y)
    (if (and (<= (lower-bound y) 0)(>= (upper-bound y) 0)) (display "error")
    (mul-interval x 
                  (make-interval (/ 1.0 (lower-bound y))(/ 1.0 (upper-bound y)))))
  )

(define (print-interval x)
    (newline)
    (display "[")
    (display (lower-bound x))
    (display "]")
    (display (upper-bound x))
  )





;pra2.7
(define (make-interval a b) (cons a b))

(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

;pra2.8
(define (sub-interval x y)
    (make-interval (- (lower-bound x)(upper-bound y))
                   (- (upper-bound x)(lower-bound y)))
  )

