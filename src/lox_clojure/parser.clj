(ns lox-clojure.parser
  (:require [lox-clojure.expr :as expr :refer :all])
  (:import (lox_clojure.expr Binary Grouping Literal Unary)))

(declare expression equality comparison addition multiplication unary primary)

(defn- current-token "Get the current token from the list of tokens" [tokens]
  (first tokens))

(defn- check-consume
  ""
  [tokens token-type error-message]
  (let [ct (current-token tokens) tt (:type ct)]
    (if (= tt token-type)
      (rest tokens)
      (throw (ex-info "Parser error"
                      {:token ct
                       :message error-message
                       :line (if (= :eof tt) "end" (:source-line-num ct))})))))

(defn- match-consume
  "If the token matches the type (individual or set), consume the token.
  Returns a vector of the token type that matched, the current token, and the remaining tokens"
  [tokens token-type]
  (let [ct (current-token tokens) tt (:type ct)]
    (if (or (and (set? token-type) (contains? token-type tt))
            (= token-type tt))
      [tt ct (rest tokens)])))

(comment
  "For all the functions below, the input is the list of the remaining tokens
   and the return value is a map with two values. The key :e is mapped to the
   expression that's being returned and the key :t is mapped to the
   remaining tokens that are being returned

   We are using keyword destructuring quite a bit to make the code simpler")

(defn- primary [tokens]
  (when-let [[tt ct rt] (match-consume tokens #{:false :true :nil :number :string :left_paren})]
    (cond
      (= :false tt) {:e (->Literal false) :t rt}
      (= :true tt)  {:e (->Literal true) :t rt}
      (= :nil tt)   {:e (->Literal nil) :t rt}
      (contains? #{:number :string} tt) {:e (->Literal (:literal ct)) :t rt}
      (= :left_paren) (let [{:keys [e t] :as _} (expression rt)]
                        {:e (->Grouping e) :t (check-consume t :right_paren
                                                             "Expect ')' after expression.")}))))

(defn- unary [tokens]
  (if-let [[tt ct rt] (match-consume tokens #{:bang :minus})]
    {:e (->Unary ct (unary rt))}
    (primary tokens)))

(defn- multiplication-* [{:keys [e t] :as e-t}]
  (let [[tt ct rt] (match-consume t #{:star :slash})]
    (if-not ct
      e-t
      (let [oe e {:keys [e t] :as _} (unary rt)]
        (recur {:e (->Binary oe ct e) :t t})))))

(defn- multiplication [tokens]
  (multiplication-* (unary tokens)))


(defn- addition-* [{:keys [e t] :as e-t}]
  (let [[tt ct rt] (match-consume t #{:minus :plus})]
    (if-not ct
      e-t
      (let [oe e {:keys [e t] :as _} (multiplication rt)]
        (recur {:e (->Binary oe ct e) :t t})))))

(defn- addition [tokens]
  (addition-* (multiplication tokens)))

(defn- comparison-* [{:keys [e t] :as e-t}]
  (let [[tt ct rt] (match-consume t #{:greater :greater_equal :less :less_equal})]
    (if-not ct
      e-t
      (let [oe e {:keys [e t] :as _} (addition rt)]
        (recur {:e (->Binary oe ct e) :t t})))))

(defn- comparison [tokens]
  (comparison-* (addition tokens)))

(defn- equality-* [{:keys [e t] :as e-t}]
  (let [[tt ct rt] (match-consume t #{:bang_equal :equal_equal})]
    (if-not ct
      e-t
      (let [oe e {:keys [e t] :as _} (comparison rt)]
        (recur {:e (->Binary oe ct e) :t t})))))

(defn- equality [tokens]
  (equality-* (comparison tokens)))

(defn- expression [tokens]
  (equality tokens))

(defn parse [tokens] (:e (expression tokens)))

