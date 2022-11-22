;左下点&右上点定义矩形
;(define (make-rec bottom-left top-right)
;    (cons bottom-left top-right)
;  )
;
;(define (bottom-left rec)
;    (car rec)
;  )
;
;(define (top-right rec)
;    (cdr rec)
;  )
;
;(define (h-rec rec)
;    (- (y-point (top-right rec))
;       (y-point (bottom-left rec)))
;  )
;(define (w-rec rec)
;    (- (x-point (top-right rec))
;       (x-point (bottom-left rec)))
;  )
;
;(define (area rec)
;    (* (h-rec rec)(w-rec rec))
;  )
;
;(define (perimeter rec)
;    (* 2 (+ (h-rec rec)(w-rec rec)))
;  )

;左下点&宽&高定义矩形
(define (make-rec bottom-left h w)
    (cons bottom-left (cons w h))
  )

(define (h-rec rec)
    (cdr (cdr rec))
  )

(define (w-rec rec)
    (car (cdr rec))
  )

(define (area-rec rec)
    (* (car(cdr rec))(cdr(cdr rec)))
  )

(define (perimeter-rec rec)
    (* 2 (+ (car (cdr rec))(cdr (cdr rec))))
  )





;点的定义
(define (make-point a b) (cons a b))

(define (x-point x)(car x))

(define (y-point x) (cdr x))

;线段的定义
(define (make-segment a b) (cons a b))

(define (start-point x) (car x))

(define (end-point x) (cdr x))

(define (midpoint-segment x)
    (make-point (/ (+ (x-point (start-point x)) (+ (x-point (end-point x)))) 2)
                (/ (+ (y-point (start-point x)) (y-point (end-point x))) 2))
  )

(define (print-point x)
    (newline)
    (display "(")
    (display (x-point x))
    (display ",")
    (display (y-point x))
    (display ")")
        )
