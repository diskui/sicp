
;empty board
(define empty-board '())


;nil
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


;flatmap
(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
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


;queen board-size
(define (queen board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter (lambda(position) (safe? position))
                    (flatmap
                      (lambda (rest-of-queens)
                        (map (lambda(new-row)
                               (adjoin-position new-row k rest-of-queens))
                             (enumerate 1 board-size)   
                             )
                        )
                      (queen-cols (- k 1))
                      )
                    )
          )
      )
    (queen-cols board-size)
  )

;check
(define (check x y)
    (let(
        (x1 (car x))
        (x2 (cadr x))
        (y1 (car y))
        (y2 (cadr y)))
      (and(not(= x1 y1))
          (not(= x2 y2))
          (not(= (abs(- y1 x1)) (abs(- y2 x2)))))
      )
  )

;safe?
(define (safe? y)
    (= 0 (accumulate +
                0 
                (map (lambda(x)(if (check (car y) x) 0 1)) (cdr y))))
  )


;adjoin position 
(define (adjoin-position r k rest-of-queens)
    (cons (list r k) rest-of-queens) 
  )



