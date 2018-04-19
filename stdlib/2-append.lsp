(define append
  (lambda args
    (let append-inner
      (lambda (lst lsts)
        (if (null? lst)
            (if (null? lsts)
                '()
                (append-inner (car lsts) (cdr lsts)))
            (cons (car lst)
                  (append-inner (cdr lst) lsts)))))
    (if (and (not (null? args))
             (not (list? (car args))))
        (car args)
        (append-inner '() args))))