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

;adjoin a element to a tree
(define (adjoin-tree x tree)
  (cond
    ((null? tree) (make-tree x nil nil))
    (= x (entry tree) tree)
    (< x (entry tree) (make-tree (entry tree) (adjoin-tree x (left-branch tree)) (right-branch tree)))
    (> x (entry tree) (make-tree (entry tree) (left-branch tree) (adjoin x(right-branch tree))))
    )
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

;nil
(define nil '())

;union of two trees
(define (union-of-trees set1 set2)
  (if (null? set1)
    set2
    (let ((result-entry (adjoin (entry set1) set2)))
      (let ((left-result (union-of-trees (left-branch set1) result-entry)))
        (union-of-trees (right-branch set1) left-result)
        )
      )
    )
  )

;intersection of two trees
(define (intersection-of-trees set1 set2)
  (cond
    ((null? set1) nil)
    ((null? set2) nil)
    ((element-of-tree? (entry set1) set2)
     (make-tree (entry set1)
                (intersection-of-trees (left-branch set1) set2)
                (intersection-of-trees (right-branch set1) set2)))
    (else (union-of-trees
            (intersection-of-trees (left-branch set1) set2)
            (intersection-of-trees (right-branch set1) set2)
            ))
    )
  )


