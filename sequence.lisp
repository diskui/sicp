
(define nil '())

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

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

;enumerate a tree
(define (enumerate-tree tree)
    (cond
        ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))
                      ))
      )
  )

;the sum of all odd numbers of a sequence
(define (sum-odd-sequence tree)
    (accumulate +
                0
                (mymap square (filter odd?
                                      (enumerate-tree tree))))
  )

;all even number of fib
(define (even-fibs n)
    (accumulate cons 
                nil
                (filter even?
                         (mymap fib
                              (enumerate 0 n))))
  )

;fib
(define (fib n)
    (define (iter a b counter)
      (if (= counter 0) 
        a
        (iter (+ a b) a (- counter 1)))
      )
    (iter 1 0 n)
  )
