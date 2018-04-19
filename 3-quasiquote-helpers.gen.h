// Autogenerated from file stdlib/3-quasiquote-helpers.lsp DO NOT TOUCH
doString(&vm,
";; See A. Bawden \"Quasiquotation in Lisp\"\n"
"\n"
"(let quasiquote? (x)\n"
"     (and (list? x) (not (null? x)) (= (car x) 'quasiquote)\n"
"          (list? (cdr x)) (not (null? (cdr x)))))\n"
"\n"
"(let unquote? (x)\n"
"     (and (list? x) (not (null? x)) (= (car x) 'unquote)\n"
"          (list? (cdr x)) (not (null? (cdr x)))))\n"
"\n"
"(let unquote-splicing? (x)\n"
"     (and (list? x) (not (null? x)) (= (car x) 'unquote-splicing)\n"
"          (list? (cdr x)) (not (null? (cdr x)))))\n"
"\n"
"(let q-data (x)\n"
"     (car (cdr x)))\n"
"\n"
"(define qq-expand (x)\n"
"     (cond ((unquote? x)\n"
"            (q-data x))\n"
"           ((unquote-splicing? x)\n"
"            (let st (stack-trace))\n"
"            (println! \"ERROR: at \"\n"
"                      (car (cdr (car st))) \":\"\n"
"                      (as-int (car (cdr (cdr (car st))))) \":\"\n"
"                      \"<proc \" (car (car st))\">: Illegal ,@\")\n"
"            (print-stack-trace st)\n"
"            (crash! true))\n"
"           ((quasiquote? x)\n"
"            (qq-expand\n"
"             (qq-expand (q-data x))))\n"
"           ((and (list? x) (not (null? x)))\n"
"            (list 'append\n"
"                  (qq-expand-list (car x))\n"
"                  (qq-expand (cdr x))))\n"
"           (true (list 'quote x))))\n"
"\n"
"(define qq-expand-list (x)\n"
"     (cond ((unquote? x)\n"
"            (list 'list (q-data x)))\n"
"           ((unquote-splicing? x)\n"
"            (q-data x))\n"
"           ((quasiquote? x)\n"
"            (qq-expand-list\n"
"             (qq-expand (q-data x))))\n"
"           ((and (list? x) (not (null? x)))\n"
"            (list 'list\n"
"                  (list 'append\n"
"                        (qq-expand-list (car x))\n"
"                        (qq-expand (cdr x)))))\n"
"           (true (list 'quote (list x)))))\n"
"",
0, false, "stdlib/3-quasiquote-helpers.lsp");