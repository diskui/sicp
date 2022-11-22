;mymap
(define (mymap proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (mymap proc (cdr items)))
      )
  )

(define nil '())

;accumulate
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
            )
      )
  )

;accumulate-n
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
      nil
      (cons (accumulate op init (mymap car seqs))
            (accumulate-n op init (mymap cdr seqs))))
  )

;dot product
(define (dot-product v w)
    (accumulate + 0 (map * v w))
  )

;matrix * vector
(define (matrix-*-vector m v)
    (map (lambda(m-col)(dot-product m-col v)) m)
  )

;transpose
(define (transpose mat)
    (accumulate-n cons nil mat)
  )

;maxtril * matrix
(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
    (map (lambda(m-row) (matrix-*-vector cols m-row)) m))
 )
