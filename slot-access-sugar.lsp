(defmacro : (obj key)
  `(get-slot ,obj ,(list 'quasiquote key)))

(defmacro :! (obj key value)
  `(set-slot! ,obj ,(list 'quasiquote key) ,value))

(defmacro ! (obj func . args)
  (append (list (list ': obj func) obj) args))

(defmacro :? (obj key)
  `(key-exists? ,obj ,(list 'quasiquote key)))
