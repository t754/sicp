(defun square (x)
  (* x x))

(defun sum-of-square (x y)
  (+ (square x)
     (square y)))

(defun 1-3 (x y z)
  (let ((sorted-lis (sort (list x y z) #'>)))
    (sum-of-square (car sorted-lis)
                   (cadr sorted-lis))))


;; (= (1-3 3 1 2 ) 13)
