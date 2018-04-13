(defmacro : (obj key)
  `(get-slot ,obj ,(list 'quasiquote key)))
(defmacro :! (obj key value)
  `(set-slot! ,obj ,(list 'quasiquote key) ,value))
(defmacro ! (obj func . args)
  (append (list (list ': obj func) obj) args))
;(defmacro range (s e)
  ;`{(start ,s) (end ,e) (step 1) (curr ,s) (alive true)
    ;(update (lambda (this)
              ;(let ret (: this curr))
              ;(if (: this alive)
                  ;(scope
                   ;(:! this curr (+ (: this curr) (: this step)))
                   ;(if (> ret (: this end))
                       ;(:! this alive false))))
              ;ret))})
;(defmacro assert (pred? . args)
  ;`(if ,pred?
       ;true
       ;(error ,@args)))
;(define list-iterator (lst)
  ;{(curr lst) (alive (not (null? lst)))
   ;(update (lambda (this)
             ;(let ret '())
             ;(if (: this alive)
                 ;(scope
                  ;(set! ret (car (: this curr)))
                  ;(:! this curr (cdr (: this curr)))
                  ;(if (null? (: this curr))
                      ;(:! this alive false))))
             ;ret))})
;(define map (proc lst)
  ;(let ret '())
  ;(let curr '())
  ;(for (elem (list-iterator lst))
       ;(if (null? ret)
           ;(scope
            ;(set! ret (cons (proc elem) ret))
            ;(set! curr ret))
           ;(scope
            ;(set-cdr! curr (cons (proc elem) '()))
            ;(set! curr (cdr curr)))))
  ;ret)
;(define foldl (proc init lst)
  ;(let ret init)
  ;(for (elem (list-iterator lst))
       ;(set! ret (proc elem ret)))
  ;ret)
;(define foldr (proc init lst)
  ;(if (null? lst)
      ;init
      ;(proc (car lst) (foldr proc init (cdr lst)))))
;(println! (map (lambda (a) (+ a 1)) '(1 2 3 4 5 6)))
;(println! (foldl cons '() '(a b c d e)))
;(println! (foldr cons '() '(a b c d e)))
;
(defmacro reverse-range (s e)
  `{(start ,s) (end ,e) (step 1) (curr ,s) (alive true)
    (update (lambda (this)
              (let ret (: this curr))
              (if (: this alive)
                  (scope
                   (:! this curr (- (: this curr) (: this step)))
                   (if (< ret (: this end))
                       (:! this alive false))))
              ret))})

(define stack-trace ()
  (let ret '())
  (for (stack-index (reverse-range (- (stack-depth) 1) 1))
       (set! ret (cons (stack-info stack-index) ret)))
  ret)

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

(define as-int (num)
  (cons '*print-as-an-integer* num))

(let print-stack-trace (trace)
     (let inner (trace level)
          (if (not (null? trace))
              (scope
               (println! "#" (as-int level) " "
                         (caar trace) " "
                         (as-int (cadar trace)) ":"
                         (as-int (caddar trace)))
               (inner (cdr trace) (+ level 1)))))
     (println! "STACK-TRACE:")
     (inner trace 0))

(defmacro error args
  (let st (gensym))
  `(scope
    (let ,st (stack-trace))
    (println! "ERROR: at " (caar ,st) ":" (cadar ,st) ": " ,@args)
    (print-stack-trace ,st)
    (crash!)))



(let test ()
     ((lambda () (error "testing the new error"))))

(scope
 (test))
