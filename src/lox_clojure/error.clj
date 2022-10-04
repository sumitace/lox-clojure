(ns lox-clojure.error)

(defn check-number-operator
  ([operator operand] (if-not (instance? Double operand) (throw (Exception.
                                                                 "Operand must be a number")))))
