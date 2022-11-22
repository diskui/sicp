
;rand
(define (rand m)
  (let ((x random-init))
    (cond
      ((eq? m 'generate)
       (lambda ()
         (set! x (rand-update x))
         x
         )
       )
      ((eq? m 'reset)
       (lambda(new-value)(set! x new-value) x))
      )
    )
  )
