(define nil '())


(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;树的平方
(define (square-tree tree)
    (cond
        ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
      )
    )

(define (square x)
    (* x x)
  )

(define (tree-map proc tree)
    (cond
      ((null? tree) nil)
      ((not (pair? tree)) (proc tree))
      (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))
      )
  )

(define (square-tree-abs tree)
    (tree-map square tree)
  )
