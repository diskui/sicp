(define (make-interval a b) (cons a b))

(define (upper-bound x)
    (max (car x) (cdr x))
  )

(define (lower-bound x)
    (min (car x)(cdr x))
  )

(define (center x)
    (/ (- (upper-bound x) (lower-bound x)) 2)
  )

(define (make-interval-center-percent c pct)
    (let ((width (* c (/ pct 100))))
    (cons (- c width) (+ c width))
      )
  )

(define (percent-tolerance x)
    (let (
        (center (/ (+ (upper-bound x)(lower-bound x)) 2))
        (width (/ (- (upper-bound x)(lower-bound x)) 2)))
    (* (/ width center) 100)    
      ))
