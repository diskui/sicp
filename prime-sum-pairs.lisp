;load
(load "prime.lisp")
(load "sequence.lisp")

;flatmap
(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
  )

;prime-sum?
(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
  )

;make-pair-sum
(define (make-pair-sum pair)
    (list (car pair) (cadr pair)(+ (car pair) (cadr pair)))
  )

;prime-sum-pairs
(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum?
                 (unique-pairs n)
                   )))

(define (permutation s)
    (if(null? s) 
      (list nil)
      (flatmap (lambda(x)
                 (map (lambda (p) (cons x p))
                      (permutation (remove x s))))
               s)
      )
  )

;remove
(define (remove item sequence)
    (filter (lambda (x) (not (= x item))) sequence)
  )


;unique pairs
(define (unique-pairs n)
   (flatmap (lambda(i) 
              (map (lambda(j) (list i j))
                   (enumerate 1 (- i 1))))
            (enumerate 1 n)) 
  )

