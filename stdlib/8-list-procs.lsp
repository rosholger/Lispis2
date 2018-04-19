(define map (proc lst)
  (let ret '())
  (let curr '())
  (for (elem (list-iterator lst))
       (if (null? ret)
           (scope
            (set! ret (cons (proc elem) ret))
            (set! curr ret))
           (scope
            (set-cdr! curr (cons (proc elem) '()))
            (set! curr (cdr curr)))))
  ret)

(define foldl (proc init lst)
  (let ret init)
  (for (elem (list-iterator lst))
       (set! ret (proc elem ret)))
  ret)

(define foldr (proc init lst)
  (if (null? lst)
      init
      (proc (car lst) (foldr proc init (cdr lst)))))