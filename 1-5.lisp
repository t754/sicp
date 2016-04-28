;; ( define (p) (p))
;; ( define (test x y)
;; (if (= x 0) 0 y))
;; (test 0 (p))



(defun p () (funcall p))
(defun test (x y)
  (if (= x 0) 0 y))
;; (test 0 (p))←これを実行

"

(作用的順序) 関数p は
p自身を再帰的に無限に呼び出してしまう.

しかし,正規順序のものだったら,
pを評価することなく,答えがでる.
"
