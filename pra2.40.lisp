
(load "prime.lisp")

(define nil '())

;map
(define (mymap proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (mymap proc (cdr items)))
      )
  )

;filter
(define (filter predicate sequence)
    (cond
        ((null? sequence) nil)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))
      )
  )

;accumulate
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
            )
      )
  )

;enumerate
(define (enumerate low high)
    (if (> low high) 
      nil
      (cons low (enumerate (+ low 1) high))
      )
  )


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


;unique pairs
(define (unique-pairs n)
   (flatmap (lambda(i) 
              (map (lambda(j) (list i j))
                   (enumerate 1 (- i 1))))
            (enumerate 1 n)) 
  )
