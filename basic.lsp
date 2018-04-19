(do-file "./slot-access-sugar.lsp")

(let a (b)
     (b))

(a
 (lambda ()
   ((lambda ()
      (cond (false 0)
            (false 0)
            (false 0)
            (false 0)
            (true
             (error! "ICE: macros and (stack-trace)/(error!) dont play nice :(")))))))
(println! "Wont run!")