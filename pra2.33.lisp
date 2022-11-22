;another form of map
(define (othermap p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence)
  )

;append
(define (apend seq1 seq2)
    (accumulate cons list2 list1)
  )

;length
(define (length sequence)
    (accumulate (lambda(x y)(+ 1 y)) 0 sequence) 
  )


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
