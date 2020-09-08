(ns lox-clojure.scanner)

(def single-char-lexemes
  {\( :left_paren, \) :right_paren, \{ :left_brace, \} :right_brace
   \, :comma, \. :dot, \- :minus, \+ :plus
   \; :semicolon, \* :star, \space :space, \return :return
   \tab :tab, \formfeed :formfeed})

(defn scan-tokens
  "Scans a line of source into tokens"
  [source]
  {:type :eof, :lexeme "", :literal nil, :location -1})

(defn parse-single-char-lexeme
  "If this character is a lexeme by itself, turn it into a token"
  [character-to-match]
  (some->> (get single-char-lexemes character-to-match)
           (assoc {:lexeme (str character-to-match)} :type)))

(defn parse-slash
  ""
  [source]
  (let [first-char (get source 0)
        source-len (count source)
        second-char (if (> (count source ) 1) (get source 1))]
    (if (= \/ first-char)
      ;; if the first char is a / and the second char is not, this is not a comment
      (if-not (= \/ second-char) {:lexeme "/", :type :slash}
              ;; if the first and second chars were /, then this is a comment
              (let [end-of-comment (or (clojure.string/index-of source "\n" 2) (count source))]
                {:lexeme (subs source 0 end-of-comment), :type :comment})))))

(defn parse-two-char-lexeme
  "If this char is up to a two-char lexeme, turn it into a token"
  [[first-char second-char]]
  (case first-char
    \! (if (= second-char \=) {:lexeme "!=", :type :bang_equal} {:lexeme "!", :type :bang})
    \= (if (= second-char \=) {:lexeme "==", :type :equal_equal} {:lexeme "!", :type :equal})
    \< (if (= second-char \=) {:lexeme "<=", :type :less_equal} {:lexeme "!", :type :less})
    \> (if (= second-char \=) {:lexeme ">=", :type :greater_equal} {:lexeme "!", :type :greater})
    nil))

(defn parse-newline
  ""
  [source]
  (if (= \newline (get source 0)) {:lexeme "\n", :type :newline}))

(defn parse-token
  "Parse the next token from the source"
  [source]
  (or (parse-single-char-lexeme (get source 0))
      (parse-two-char-lexeme (vec (subs source 0 2)))
      (parse-slash source)
      (parse-newline source)
      {:lexeme source, :type :404}))

(defn parse-tokens
  "Recursively parse the remaining source into tokens"
  [parsed-tokens source]
  (if (clojure.string/blank? source) parsed-tokens
      (let [this-token (parse-token source)
            parsed-tokens (conj parsed-tokens this-token)
            this-token-len (count (:lexeme this-token))
            remaining-source (if (> (count source) this-token-len) (subs source this-token-len))]
        (recur parsed-tokens remaining-source))))

