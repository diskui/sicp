

;nil
(define nil '())

;make a tree
(define (make-tree entry left right)
  (list entry left right)
  )

;entry of a tree
(define (entry tree)
  (car tree)
  )

;left branch of a tree
(define (left-branch tree)
  (cadr tree)
  )

;right branch of a tree
(define (right-branch tree)
  (caddr tree)
  )

;is a element one of a tree?
(define (element-of-tree? x tree)
  (cond
    ((null? tree) #f)
    ((= (entry tree) x) #t)
    ((< x (entry tree))(element-of-tree? x (left-brance tree)))
    ((> x (entry tree))(element-of-tree? x (right-brance tree)))
    )
  )

;adjoin a element to a tree
(define (adjoin-tree x tree)
  (cond
    ((null? tree) (make-tree x nil nil))
    (= x (entry tree) tree)
    (< x (entry tree) (make-tree (entry tree) (adjoin-tree x (left-branch tree)) (right-branch tree)))
    (> x (entry tree) (make-tree (entry tree) (left-branch tree) (adjoin x(right-branch tree))))
    )
  )
