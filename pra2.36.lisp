
(define nil '())

;map
(define (mymap proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (mymap proc (cdr items)))
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

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
      nil
      (cons (accumulate op init (mymap car seqs))
            (accumulate-n op init (mymap cdr seqs))))
  )
