
;(define a (lambda (b c) b)) (a true false)

;;; Fails
;((lambda (b)
;   ((lambda (b) b)
;    b)) true)

;;; ((lambda (a) (make-object (((quote a) 1))) * 86 a) true) returns an
;;; object. Bizarely if all make-object are replaced with 1s true is
;;; returned

(define testFunc
  (lambda ()
    (make-object (((quote a) 1)))))