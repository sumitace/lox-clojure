(ns lox-clojure.ast-print
  (:require [lox-clojure.expr :as expr :refer :all])
  (:import (lox_clojure.expr Binary Grouping Literal Unary)))

(defprotocol AstPrintable
  (ast-print [this]))

(defn- print-parens
  "Prints the AST in lisp-type form"
  [name & expressions]
  (str "(" name " " (reduce print-str (map ast-print expressions)) ")"))

(extend-protocol AstPrintable
  Binary
  (ast-print [{:keys [operator left right]}] (print-parens (:lexeme operator) left right))
  Grouping
  (ast-print [{:keys [expression]}] (print-parens "group" expression))
  Literal
  (ast-print [{:keys [value]}] (if-not value "nil" (str value)))
  Unary
  (ast-print [{:keys [operator right]}] (print-parens (:lexeme operator) right)))

