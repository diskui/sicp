(define nil '())

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;树的缩放
(define (scale-tree tree factor)
  (cond
    ((null? tree) nil)
    ((not (pair? tree)) (* factor tree))
    (else (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))
    )
  )

;表的映射
(define (mymap proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (mymap proc (cdr items)))
      )
  )

;树的缩放(map)
(define (scale-tree-map tree factor)
    (mymap (lambda (sub-tree) (if (not(pair? sub-tree)) (* factor sub-tree) (scale-tree-map sub-tree factor))) tree)
  )

;树的平方
(define (square-tree tree)
    (cond
        ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))
      )
  )

;树的平方(map)
(define (square-tree-map tree)
  (mymap (lambda (sub-tree) 
           (cond ((null? sub-tree) nil) 
                 ((not (pair? sub-tree))(square sub-tree))
                 (else (square-tree-map sub-tree)))) 
         tree)
  )

(define (square x)
    (* x x)
  )
