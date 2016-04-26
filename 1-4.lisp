;; (define ( a-plus-abs-b a b)
;;     ((if (> b 0) + -) a b))
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) (symbol-function '+)  (symbol-function '-)) a b))
