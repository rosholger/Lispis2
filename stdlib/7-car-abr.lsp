(defmacro cadr (lst)
  `(car (cdr ,lst)))

(defmacro caar (lst)
  `(car (car ,lst)))

(defmacro cadar (lst)
  `(cadr (car ,lst)))

(defmacro cdar (lst)
  `(cdr (car ,lst)))

(defmacro caddar (lst)
  `(cadr (cdar ,lst)))

(defmacro caaar (lst)
  `(car (caar ,lst)))

(defmacro cadaar (lst)
  `(cadr (caar lst)))