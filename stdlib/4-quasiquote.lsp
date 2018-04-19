;; See 3-quasiquote-helpers.lsp

(defmacro quasiquote (arg)
  (qq-expand arg))