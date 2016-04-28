(defpackage sicp
  (:use :common-lisp)
  (:export #:square
           #:average
           #:improve
           #:good-enough?
           #:sqrt)
  (:shadow #:sqrt))

(in-package :sicp)

(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        (t else-clause)))

;; ↑ 引数評価されて,無限再帰に陥る
;; ↓ 引数評価されない ちゃんと収縮する

(defmacro new2-if (a b d)
  `(cond (,a ,b)
         (t ,d)))



(defun sqrt-iter (guess x)
  (new2-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                    x)))
(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))
(defun square (x)
  (* x x))

(defun sqrt (x)
  (sqrt-iter 1.0 x))
