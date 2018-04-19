;; See A. Bawden "Quasiquotation in Lisp"

(let quasiquote? (x)
     (and (list? x) (not (null? x)) (= (car x) 'quasiquote)
          (list? (cdr x)) (not (null? (cdr x)))))

(let unquote? (x)
     (and (list? x) (not (null? x)) (= (car x) 'unquote)
          (list? (cdr x)) (not (null? (cdr x)))))

(let unquote-splicing? (x)
     (and (list? x) (not (null? x)) (= (car x) 'unquote-splicing)
          (list? (cdr x)) (not (null? (cdr x)))))

(let q-data (x)
     (car (cdr x)))

(define qq-expand (x)
     (cond ((unquote? x)
            (q-data x))
           ((unquote-splicing? x)
            (let st (stack-trace))
            (println! "ERROR: at "
                      (car (cdr (car st))) ":"
                      (as-int (car (cdr (cdr (car st))))) ":"
                      "<proc " (car (car st))">: Illegal ,@")
            (print-stack-trace st)
            (crash! true))
           ((quasiquote? x)
            (qq-expand
             (qq-expand (q-data x))))
           ((and (list? x) (not (null? x)))
            (list 'append
                  (qq-expand-list (car x))
                  (qq-expand (cdr x))))
           (true (list 'quote x))))

(define qq-expand-list (x)
     (cond ((unquote? x)
            (list 'list (q-data x)))
           ((unquote-splicing? x)
            (q-data x))
           ((quasiquote? x)
            (qq-expand-list
             (qq-expand (q-data x))))
           ((and (list? x) (not (null? x)))
            (list 'list
                  (list 'append
                        (qq-expand-list (car x))
                        (qq-expand (cdr x)))))
           (true (list 'quote (list x)))))