

;make a tree
(define (make-tree entry left right)
  (list entry left right)
  )


;nil
(define nil '())

;list -> tree
(define (list->tree elements)
  (car (partial-tree elements (length elements)))
  )


;partial a tree with first n elements of a list
(define (partial elements n)
  (if (= n 0) (cons nil elements)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial elements lest-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ 1 left-size))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial (cdr non-left-elts) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree) remaining-elts)))
          )
        )
      )
    )
  )
