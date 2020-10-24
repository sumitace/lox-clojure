(ns lox-clojure.interpreter
  (:require [lox-clojure.expr :as expr :refer :all])
  (:import (lox_clojure.expr Binary Grouping Literal Unary)))

(defprotocol Interpretable
  (interpret [this]))

(defn number-operand?
  [operand]
  (double? operand))

(extend-protocol Interpretable
  Literal
  (interpret [{:keys [value]}] value)

  Grouping
  (interpret [{:keys [expression]}] (interpret expression))

  Unary
  (interpret [{:keys [operator right]}]
    (let [right (interpret right)]
      (case (:type operator)
       :bang (boolean right)
       :minus (- right))))

  Binary
  (interpret [{:keys [operator left right]}]
    (let [left (interpret left)
          right (interpret right)]
      (case (:type operator)
       ;; comparison operators
       :greater (>= left right)
       :less (< left right)
       :less_equal (<= left right)
       ;; arithmetic operators
       :minus (- left right)
       :plus (if (and (double? left) (double? right)) (+ left right) (str left right))
       :slash (/ left right)
       :star (* left right)
       ;; equality operators
       :bang_equal (not= left right)
       :equal_equal (= left right)))))

