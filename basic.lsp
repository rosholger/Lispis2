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

(quote (1 (2) () a b c 3 4))