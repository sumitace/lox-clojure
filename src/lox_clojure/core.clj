(ns lox-clojure.core
  (:require [lox-clojure.scanner :as scanner]
            [lox-clojure.parser :as parser]
            [lox-clojure.ast-print :as ast-print])
  (:gen-class))

(defn run
  "To run the code, whether in REPL or file mode"
  [source]
  (let [tokens (scanner/scan-tokens source)
        expression (parser/parse tokens)]
    (ast-print/ast-print expression)))

(defn run-prompt
  "To start a REPL"
  []
  (doseq [line (line-seq (java.io.BufferedReader. *in*))] (run line)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (case (count args)
    0 (run-prompt)
    1 (println "not implemented")
    (do (println "Usage: jlox [script]") (System/exit 64))))

