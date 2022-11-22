;点的定义
(define (make-point a b) (cons a b))

(define (x-point x)(car x))

(define (y-point x) (cdr x))

;线段的定义
(define (make-segment a b) (cons a b))

(define (start-point x) (car x))

(define (end-point x) (cdr x))

(define (midpoint-segment x)
    (make-point (/ (+ (x-point (start-point x)) (+ (x-point (end-point x)))) 2)
                (/ (+ (y-point (start-point x)) (y-point (end-point x))) 2))
  )

(define (print-point x)
    (newline)
    (display "(")
    (display (x-point x))
    (display ",")
    (display (y-point x))
    (display ")")
        )
