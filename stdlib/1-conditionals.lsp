(defmacro and args
  (if (null? (cdr args))
      (car args)
      (list 'if (car args)
            (cons 'and (cdr args)) 'false)))


(defmacro or args
  (if (null? (cdr args))
      (car args)
      (list 'if (car args)
            'true (cons 'or (cdr args)))))

(defmacro cond branches
  (let inner
    (lambda (branches)
      (if (null? branches)
          '()
          (if (null? (cdr branches))
              (append '(if) (list (car (car branches)))
                      (list (append '(scope) (cdr (car branches)))))
              (append '(if) (list (car (car branches)))
                      (list (append '(scope) (cdr (car branches))))
                      (list (inner (cdr branches))))))))
  (inner branches))