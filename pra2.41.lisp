

(define nil '())

;unique-tuples
(define (unique-tuples n k)
  (cond
    ((< n k) nil)
    ((= k 0) (list nil))
    (else (append (unique-tuples (- n 1) k)
                  (map (lambda(tuple) (cons n tuple))
                       (unique-tuples (- n 1) (- k 1)))))
    )
  )

;triple of sum
(define (triple-of-sum s n)
    (filter (lambda(seq) (= (accumulate + 0 seq) s))
            (unique-tuples n 3))
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

;表的追加
(define (append list1 list2)
    (if (null? list1)
      list2
      (cons (car list1)(append (cdr list1) list2))
      )
  )
