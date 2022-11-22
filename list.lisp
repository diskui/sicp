;返回表的第n项
(define (list-ref items n)
    (if (= 0 n)
    (car items)
    (list-ref (cdr items) (- n 1))
      )
  )

;求表长
(define (length items)
    (define (iter a count)
        (if (null? a)
            count
            (iter (cdr a) (+ count 1))
          )
      )
    (iter items 0)
  )

;表的追加
(define (append list1 list2)
    (if (null? list1)
      list2
      (cons (car list1)(append (cdr list1) list2))
      )
  )

;表的映射
(define (mymap proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (mymap proc (cdr items)))
      )
  )
;表的缩放
(define (scale-list items factor)
    (mymap (lambda(x)(* x factor)) items)
  )

;叶子计数
(define (count-leaves x)
    (cond ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x))
             (count-leaves (cdr x))))
          )
  )

;树的缩放
(define (scale-tree tree factor)
    (cond
        ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))
      )
  )
;输的缩放实例 放大/缩小
(define (scale-tree-map tree factor)
    (mymap (lambda(sub-tree)
             (if (pair? sub-tree) (scale-tree-map sub-tree factor) (* sub-tree factor))) tree)
  )

;表的最后一项
(define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))
      )
  )

;reverse颠倒表的顺序
(define nil '())
(define (reverse x)
    (define (iter items result)
        (if (null? items)
            result
            (iter (cdr items) (cons (car items) result))
          )
      )
    (iter x nil)
  )

;求与表头同模的所有数
(define (same-parity first . rest)
    (define (same-parity-iter source dist remainder-val)
        (if (null? source)
            dist
            (same-parity-iter (cdr source)
                              (if (= (remainder (car source) 2) remainder-val)
                                (append dist (list (car source)))
                                dist
                                )
                              remainder-val
                              )
          )
            )
    (same-parity-iter rest (list first) (remainder first 2))
  )

;表的每一项求平方
(define (square-list items)
    (if (null? items)
        nil
        (cons (square (car items)) (square-list (cdr items)))
      )
  )

;表的每一项求平方(map)
(define (square-list-other items)
    (mymap square items)
  )

(define (square x)
    (* x x)
  )

;表的每一项操作，不储存值，有error
(define (for-each proc items)
  (proc (car items))
  (cond
    ((null? (cdr (items))) #t)
    (else (for-each proc (list (cdr items))))
        )
  )

;颠倒树&子树的顺序
(define (deep-reverse x)
    (define (iter items result)
        (cond 
          ((null? items) result)
          ((pair? (car items)) (iter (cdr items) (cons (deep-reverse (car items)) result)))
          (else (iter (cdr items) (cons (car items) result)))
              )
          )
    (iter x nil)
  )
