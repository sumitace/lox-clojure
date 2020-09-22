(ns lox-clojure.expr)

(defrecord Binary [left operator right])
(defrecord Grouping [expression])
(defrecord Literal [value])
(defrecord Unary [operator right])

(defprotocol AstPrintable
  (ast-print [this]))

(extend-type Binary
  AstPrintable
  (ast-print [{:keys [operator left right]}] (print-parens (:lexeme operator) left right)))

(extend-type Grouping
  AstPrintable
  (ast-print [{:keys [expression]}] (print-parens "group" expression)))

(extend-type Literal
  AstPrintable
  (ast-print [{:keys [value]}] (if-not value "nil" (str value))))

(extend-type Unary
  AstPrintable
  (ast-print [{:keys [operator right]}] (print-parens (:lexeme operator) right)))

(defn- print-parens
  "Prints the AST in lisp-type form"
  [name & expressions]
  (str "(" name " " (reduce print-str (map ast-print expressions)) ")"))

