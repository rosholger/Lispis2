;(define a (lambda (b c) b)) (a true false)

(define testFunc2
  (lambda (b)
    (define testFunc
      (lambda () b))))
(testFunc2 -.5)
(let a
  (lambda (b)
    (lambda (s v)
      (if (= s (quote set))
          (set! b v)
          b))))

(let z (a true))

;(let quasi-quote-inner (lambda (lst)
;  (cond ((null? lst) '())
;        ((= (car lst) 'unquote-splice)
;          (concat (car lst) (quasi-quote-inner (cdr lst))))
;        ((= (car lst) 'unquote)
;          (cons (car lst) (quasi-quote-inner (cdr lst))))
;        ((= (type (car lst)) 'list)
;          (cons (list 'quasi-quote (car lst))
;                (quasi-quote-inner (cdr lst))))
;(defmacro quasi-quote (lst)
;  (cond ((= (type lst) 'list)
;          (quasi-quote-inner lst))))

;`(a b ,c) == (list (quasi-quote a) (quasi-quote b) c) ==
; (list (quote a) (quote b) c)
;`(a (b ,f) ,@(c d)) ==
; (list (quasi-quote a)
;       (quasi-quote (b (unquote f))) c d) ==
; (list (quote a) (list (quasi-quote b) f) c d) ==
; (list (quote a) (list (quote b) f) c d)

(quote (1 (2) () a b c 3 4))

(defmacro test-macro (a b c)
  (< a b c))

(test-macro 1 2 3)