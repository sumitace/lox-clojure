(ns lox-clojure.expr)

(defrecord Binary [left operator right])
(defrecord Grouping [expression])
(defrecord Literal [value])
(defrecord Unary [operator right])

