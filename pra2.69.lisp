



;memq
(define (memq item x)
  (cond
    ((null? x) #f)
    ((eq? (car x) item) x)
    (else (memp item (cdr x)))
    )
  )

;nil
(define nil '())

;make a leaf
(define (make-leaf sym weight)
  (list 'leaf sym weight)
  )

;is a leaf?
(define (leaf? x)
  (eq? 'leaf (car x))
  )

;the symbol of a leaf
(define (symbol-leaf x) (cadr x))


;the weight of a leaf
(define (weight-leaf x) (caddr x))

;make a code tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        ;(+ (weight left) (weight right)))
        (+ (weight left) (weight right))
        )
  )

;branchs of a tree
(define (left-branch tree)(car tree))

(define (right-branch tree)(cadr tree))
;
;;the symbols of a tree
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)
    )
  )

;;the weight of a tree
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)
    )
  )
;
;;decode
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      nil
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))
      )
    )
  (decode-1 bits tree)
  )

;choose branch
(define (choose-branch bit tree)
  (cond
    ((= bit 0) (left-branch tree))
    ((= bit 1) (right-branch tree))
    (else (error "bad bit -- CHOOSE BRANCH" bit))
    )
  )

;;adjoin
(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))
    )
  )

;;make pairs a leaf set
(define (make-leaf-set pairs)
  (if (null? pairs)
    nil
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs)))
      )
    )
  )

;;pra2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;pra2.68
;encode
(define (encode message tree)
  (if (null? tree)
    nil
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))
    )
  )

;encode-symbol
(define (encode-symbol symbol tree)
  (if (leaf? tree)
    (if (eq? (symbol-leaf tree) symbol)
      nil
      (error "symbol not in tree" symbol))
    (if (memq symbol (symbols (left-branch tree)))
      (cons 0 (encode-symbol symbol (left-branch tree)))
      (cons 1 (encode-symbol symbol (right-branch tree))))
    )
  )



;pra2.69
;generate a huffman tree
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
  )

;successive-merge
(define (successive-merge set)
  (let (
        (lightest-tree (car set))
        (heavier-tree (cdr set))
        )
    (if (null? heavier-tree)
      lightest-tree
      (successive-merge (adjoin-set (make-code-tree lightest-tree (car heavier-tree))(cdr heavier-tree)))))
  )

