(ns lox-clojure.scanner)

(def single-char-lexemes
  {\( :left_paren, \) :right_paren, \{ :left_brace, \} :right_brace
   \, :comma, \. :dot, \- :minus, \+ :plus
   \; :semicolon, \* :star})

(defn scan-tokens
  "Scans a line of source into tokens"
  [source]
  {:type :eof, :lexeme "", :literal nil, :location -1})

(defn parse-single-char-lexeme
  "If this character is a lexeme by itself, turn it into a token"
  [character-to-match]
  (some->> (get single-char-lexemes character-to-match)
           (assoc {:lexeme (str character-to-match)} :type)))

(defn parse-two-char-lexeme
  "If this char is up to a two-char lexeme, turn it into a token"
  [[first-char second-char]]
  (case first-char
    \! (if (= second-char \=) {:lexeme "!=", :type :bang_equal} {:lexeme "!", :type :bang})
    \= (if (= second-char \=) {:lexeme "==", :type :equal_equal} {:lexeme "!", :type :equal})
    \< (if (= second-char \=) {:lexeme "<=", :type :less_equal} {:lexeme "!", :type :less})
    \> (if (= second-char \=) {:lexeme ">=", :type :greater_equal} {:lexeme "!", :type :greater})))

(defn parse-token
  "Parse the next token from the source"
  [source]
  (or (parse-single-char-lexeme (get source 0))
      (parse-two-char-lexeme (vec (subs source 0 2)))))

(defn parse-tokens
  "Recursively parse the remaining source into tokens"
  [parsed-tokens source]
  (if (clojure.string/blank? source) parsed-tokens
      (let [this-token (parse-token source)
            parsed-tokens (conj parsed-tokens this-token)
            this-token-len (count (:lexeme this-token))
            remaining-source (if (> (count source) this-token-len) (subs source this-token-len))]
        (recur parsed-tokens remaining-source))))

