
;nil
(define nil '())

;element of set
(define (element-of-set? x set)
    (cond
        ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))
      )
  )

;adjoin set
(define (adjoin-set x set)
    (cons x set)
  )

;intersection of sets
(define (intersection-set set1 set2)
  )


;union of sets
(define (union-set set1 set2)
  (append set1 set2)
  )

