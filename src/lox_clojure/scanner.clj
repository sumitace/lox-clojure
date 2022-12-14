(ns lox-clojure.scanner)

(defrecord Token [type lexeme literal location])

(defn- make-token
  ([type lexeme] (->Token type lexeme nil nil))
  ([type lexeme literal] (->Token type lexeme literal nil)))

(def single-char-lexemes
  {\( :left_paren, \) :right_paren, \{ :left_brace, \} :right_brace
   \, :comma, \. :dot, \- :minus, \+ :plus
   \; :semicolon, \* :star, \space :space, \return :return
   \tab :tab, \formfeed :formfeed})

(def keyword-lexemes
  {"and" :and, "class" :class, "else" :else, "false" :false
   "for" :for, "fun", :function, "if" :if, "nil" :nil
   "or" :or, "print" :print, "return" :return, "super" :super
   "this" :this, "true" :true, "var" :var, "while" :while})

(def ignored-token-types
  #{:space :return :tab :formfeed :newline :comment})

(defn- parse-single-char-lexeme
  "If this character is a lexeme by itself, turn it into a token"
  [character-to-match]
  (some-> (get single-char-lexemes character-to-match)
          (make-token (str character-to-match))))

(defn- parse-slash-lexeme
  "Parse comments or slash"
  [source]
  (let [first-char (get source 0)
        source-len (count source)
        second-char (if (> (count source ) 1) (get source 1))]
    (if (= \/ first-char)
      ;; if the first char is a / and the second char is not, this is not a comment
      (if-not (= \/ second-char) (make-token :slash "/")
              ;; if the first and second chars were /, then this is a comment
              (let [end-of-comment (or (clojure.string/index-of source "\n" 2) (count source))]
                (make-token :comment (subs source 0 end-of-comment)))))))

(defn- parse-two-char-lexeme
  "If this char is up to a two-char lexeme, turn it into a token"
  [[first-char second-char]]
  (case first-char
    \! (if (= second-char \=) (make-token :bang_equal "!=") (make-token :bang "!"))
    \= (if (= second-char \=) (make-token :equal_equal "==") (make-token :equal "="))
    \< (if (= second-char \=) (make-token :less_equal "<=") (make-token :less "<"))
    \> (if (= second-char \=) (make-token :greater_equal ">=") (make-token :greater ">"))
    nil))

(defn- parse-newline-lexeme
  "Parse newlines"
  [source]
  (if (= \newline (get source 0)) (make-token :newline "\n")))

(defn- parse-string-lexeme
  "Parse strings"
  [source]
  (if (= \" (get source 0))
    (let [end-of-string (clojure.string/index-of source \" 1)]
      (if-not end-of-string
        (make-token :error-unterminated-string source)
        (make-token :string (subs source 0 (inc end-of-string)) (subs source 1 end-of-string))))))

(defn- this-parse-double
  "Parse as double - return nil if not"
  [s]
  (try (Double/parseDouble s) (catch Exception e)))

(defn- is-int?
  "True if char is int"
  [c]
  (let [c (int c)] (and (>= c (int \0)) (<= c (int \9)))))

(defn- parse-number-lexeme
  "Parse numbers"
  [source]
  (if (is-int? (get source 0))
    (let [lexeme (str (reduce
                       ;; reduce until the reduced string no longer parses as a double
                       (fn [s i] (let [si (str s i)] (if (this-parse-double si) (str si) (reduced s))))
                       source))
          ;; drop non-int chars from the end, cuz java can parse things like "23." and "23.4\n\n"
          ;; as doubles
          lexeme (apply str (reverse (drop-while (partial (complement is-int?))
                                                 (reverse (vec lexeme)))))]
      (make-token :number lexeme (this-parse-double lexeme)))))

(defn- is-alpha?
  "Returns true if the char is alphabetic"
  [c]
  (let [c (int c)] (or (and (>= c (int \a)) (<= c (int \z)))
                       (and (>= c (int \A)) (<= c (int \Z)))
                       (= c (int \_)))))

(defn- is-alphanumeric?
  "Returns true if the char is alphanumeric"
  [c]
  (or (is-int? c) (is-alpha? c)))

(defn- parse-identifier-lexemes
  ""
  [source]
  (if (is-alpha? (get source 0))
    (let [lexeme (str (reduce
                   ;; reduce until the reduced string is not alphanumeric
                   (fn [s i] (let [si (str s i)] (if (is-alphanumeric? i) si (reduced s))))
                   source))]
      (make-token :identifier lexeme))))

(defn- parse-identifier-keyword-lexemes
  ""
  [source]
  (if-some [token (parse-identifier-lexemes source)]
    (if-some [type (get keyword-lexemes (:lexeme token))]
      (assoc token :type type)
      token)))

(defn- parse-token
  "Parse the next token from the source"
  [source]
  (or (parse-single-char-lexeme (get source 0))
      (if (>= (count source) 2) (parse-two-char-lexeme (vec (subs source 0 2))))
      (parse-slash-lexeme source)
      (parse-newline-lexeme source)
      (parse-string-lexeme source)
      (parse-number-lexeme source)
      (parse-identifier-keyword-lexemes source)
      (make-token :404 source)))

(defn- parse-tokens
  "Recursively parse the remaining source into tokens"
  [parsed-tokens source source-line-num]
  (if (clojure.string/blank? source) {:tokens parsed-tokens, :last-line-num source-line-num}
      (let [this-token (parse-token source)
            this-token (assoc this-token :source-line-num source-line-num)
            had-newline? (= :newline (:type this-token))
            next-source-line-num (if had-newline? (inc source-line-num) source-line-num)
            parsed-tokens (conj parsed-tokens this-token)
            this-token-len (count (:lexeme this-token))
            remaining-source (if (> (count source) this-token-len) (subs source this-token-len))]
        (recur parsed-tokens remaining-source next-source-line-num))))

(defn- remove-useless-tokens
  "Remove whitespace, comments"
  [tokens]
  (vec (remove #(contains? ignored-token-types (:type %)) tokens)))

(defn scan-tokens
  "Scans a line of source into tokens"
  [source]
  (let [{:keys [tokens last-line-num]} (parse-tokens [] source 0)
        tokens (remove-useless-tokens tokens)]
    (conj tokens (->Token :eof nil nil last-line-num))))

