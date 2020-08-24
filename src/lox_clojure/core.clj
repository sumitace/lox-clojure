(ns lox-clojure.core
  (:gen-class))

(defn run-prompt
  "To start a REPL"
  []
  (doseq [line (line-seq (java.io.BufferedReader. *in*))] (println line)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (case (count args)
    0 (run-prompt)
    1 (println "not implemented")
    (do (println "Usage: jlox [script]") (System/exit 64)))
  (println "Hello, World!"))

