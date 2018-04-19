;(define print-stack-trace (trace)
  ;(let inner (trace level)
       ;(if (not (null? trace))
           ;(scope
            ;(let proc-name (caar trace))
            ;(let file-name (cadar trace))
            ;(let call-line (as-int (caddar trace)))
            ;(let def-line (as-int (cadddar trace)))
            ;(println! "#" (as-int level) " "
                      ;proc-name " at "
                      ;file-name ":"
                      ;call-line)
            ;(inner (cdr trace) (+ level 1)))))
  ;(println! "STACK-TRACE:")
  ;(inner trace 0))