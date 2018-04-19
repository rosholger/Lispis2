(defmacro for (init . body)
  (let iter (gensym))
  (let for-label (gensym))
  `(scope
    (let ,iter ,(car (cdr init)))
    (label ,for-label)
    (if (get-slot ,iter 'alive)
        (scope
         (let ,(car init) ((get-slot ,iter 'update) ,iter))
         ,@body
         (go ,for-label)))))