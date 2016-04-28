(defun my/sqrt-iter (guess x)
  (new2-if (my/good-enough? guess x)
      guess
      (my/sqrt-iter (my/improve guess x)
                    x)))

(defun my/improve (guess x)
  (my/average guess (/ x guess)))

(defun my/average (x y)
  (/ (+ x y) 2))

(defun my/good-enough? (guess x)
  (< (abs (- (my/square guess) x)) 0.001))

(defun my/square (x)
  (* x x))

(defun my/sqrt (x)
  (my/sqrt-iter 1.0 x))


(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        (t else-clause)))

;; ↑ 引数評価されて,無限再帰に陥る
;; ↓ 引数評価されない ちゃんと収縮する

(defmacro new2-if (a b d)
  `(cond (,a ,b)
         (t ,d)))
