;; Change to a Cpp function, and make error! automagically do this stuff
;(define stack-trace ()
  ;(let ret '())
  ;(for (stack-index (reverse-range (- (stack-depth) 1) 1))
       ;(set! ret (cons (stack-info stack-index) ret)))
  ;ret)

;; Define in Cpp when we handle Cpp defined macros
(defmacro error! args
  `(scope
    (crash! (list ,@args))))

(defmacro assert! (pred . msg)
  `(if (not ,pred)
       (error! "assert " ',pred " failed" (newline) ,@msg)))
