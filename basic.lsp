(define sasd ()
  (lambda () (println! "blah"))
  (println! "Called from slot-access-sugar defined in basic"))
(do-file "./slot-access-sugar.lsp")

(let a {(asd 1)})
(let b {(*parent* a)})
(println! (key-exists? b 'asd) " (: a asd) is " (: a asd) " (: b asd) is " (: b asd))
(remove-slot! b 'asd)
(println! (:? b asd) " " b " " (:? a asd) " " a " " {(a 1) (b 2) (c 3)})


(let a (b)
     (b))
(println! test)