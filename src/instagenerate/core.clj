(ns instagenerate.core
  (:refer-clojure :exclude [record? ==])
  (:require [instaparse.core :as insta]
            [instaparse.combinators :as comb])
  (:use clojure.core.logic))

(defn has-tag?
  [combinator]
  (= :hiccup (get-in combinator [:red :reduction-type])))

(defn hide?
  [combinator]
  (:hide combinator))

(declare partial-parseo)

(defmulti combinator-parseo (fn [x & args]
                              (:tag x)))

(defmethod combinator-parseo :default
  [& args]
  (throw (Exception. (pr-str args))))

(defmethod combinator-parseo :string
  [{s :string :keys [hide]} grammar strings parse-tree remaining-strings remaining-parse-tree]
  (fresh []
         (== strings (reduce #(lcons %2 %1) remaining-strings (reverse s)))
         (if-not hide
           (conso s remaining-parse-tree parse-tree)
           (== remaining-parse-tree parse-tree))))

(defmethod combinator-parseo :nt
  [{k :keyword :keys [hide]} grammar strings parse-tree remaining-strings remaining-parse-tree]
  (cond
    ;hide
    ;(fresh [ptree-first]
    ;       (== remaining-parse-tree parse-tree)
    ;       (partial-parseo (get grammar k) strings ptree-first remaining-strings ()))
    
    (and (has-tag? (get grammar k)) (not hide))
    (fresh [ptree-first]
           ;(conso ptree-first remaining-parse-tree parse-tree)
           ;(partial-parseo (get grammar k) strings parse-tree remaining-strings ())
           (conso ptree-first remaining-parse-tree parse-tree)
           (partial-parseo (cond-> (get grammar k)
                                   hide (assoc :hide true))
                           grammar strings ptree-first remaining-strings ()))
    
    :else
    (fresh []
           (partial-parseo (cond-> (get grammar k)
                                   hide (assoc :hide true))
                           grammar strings parse-tree remaining-strings remaining-parse-tree))))

(defmethod combinator-parseo :cat
  [{combs :parsers
    :keys [hide]
    :as combinator} grammar strings parse-tree remaining-strings remaining-parse-tree]
  (let [combs (if hide
                (map #(assoc % :hide true) combs)
                combs)]
    (cond
      (empty? combs) (fresh []
                            (== strings remaining-strings)
                            (== parse-tree remaining-parse-tree))
      :else (fresh [rem-strings0 rem-pt0]
                   (partial-parseo (first combs)
                                   grammar strings parse-tree rem-strings0 rem-pt0)
                   (partial-parseo (update combinator :parsers rest)
                                   grammar rem-strings0 rem-pt0 remaining-strings remaining-parse-tree)))))

(defmethod combinator-parseo :alt
  [{combs :parsers
    :keys [hide]
    :as combinator} grammar strings parse-tree remaining-strings remaining-parse-tree]
  (let [combs (if hide
                (map #(assoc % :hide true) combs)
                combs)]
    (cond
      (empty? combs) u#
      :else (conde
              [(partial-parseo (first combs) grammar strings parse-tree remaining-strings remaining-parse-tree)]
              [(partial-parseo (update combinator :parsers rest) grammar strings parse-tree remaining-strings remaining-parse-tree)]))))

(defmethod combinator-parseo :star
  [{comb :parser :keys [hide] :as combinator} grammar strings parse-tree remaining-strings remaining-parse-tree]
  (let [comb (if hide
               (assoc comb :hide true)
               comb)]
    (conde
      [(== strings remaining-strings)
       (== parse-tree remaining-parse-tree)]
      [(fresh [rem-strings0 rem-pt0]
              (partial-parseo comb grammar strings parse-tree rem-strings0 rem-pt0)
              (combinator-parseo combinator grammar rem-strings0 rem-pt0 remaining-strings remaining-parse-tree))])))

(defmethod combinator-parseo :plus
  [{comb :parser :keys [hide] :as combinator} grammar strings parse-tree remaining-strings remaining-parse-tree]
  (let [comb (if hide
               (assoc comb :hide true)
               comb)]
    (conde
      [(partial-parseo comb grammar strings parse-tree remaining-strings remaining-parse-tree)]
      [(fresh [rem-strings0 rem-pt0]
              (partial-parseo comb grammar strings parse-tree rem-strings0 rem-pt0)
              (combinator-parseo combinator grammar rem-strings0 rem-pt0 remaining-strings remaining-parse-tree))])))

(defmethod combinator-parseo :opt
  [{comb :parser :keys [hide] :as combinator} grammar strings parse-tree remaining-strings remaining-parse-tree]
  (let [comb (if hide
               (assoc comb :hide true)
               comb)]
    (conde
      [(== strings remaining-strings)
       (== parse-tree remaining-parse-tree)]
      [(partial-parseo comb grammar strings parse-tree remaining-strings remaining-parse-tree)])))

(defmethod combinator-parseo :look
  [{comb :parser :keys [hide] :as combinator} grammar strings parse-tree remaining-strings remaining-parse-tree]
  (let [comb (assoc comb :hide true)]
    (fresh [fake-remaining-strings]
           (== strings remaining-strings)
           (== parse-tree remaining-parse-tree)
           (partial-parseo comb grammar strings parse-tree fake-remaining-strings parse-tree))))

(defmethod combinator-parseo :epsilon
  [_ strings parse-tree remaining-strings remaining-parse-tree]
  (fresh []
         (== strings remaining-strings)
         (== parse-tree remaining-parse-tree)))

(defn partial-parseo
  [combinator grammar strings parse-tree remaining-strings remaining-parse-tree]
  (cond
    (and (has-tag? combinator) (not (:hide combinator)))
    ; parse-tree = [:a & a-stuff]
    ; remaining-parse-tree = ()
    ; (partial-parse combinator-without-tag strings) = a-stuff
    (fresh [rparse-tree]
           (conso (get-in combinator [:red :key]) rparse-tree parse-tree)
           (emptyo remaining-parse-tree)
           (partial-parseo (dissoc combinator :red) grammar strings rparse-tree remaining-strings remaining-parse-tree))
    
    :else (combinator-parseo combinator grammar strings parse-tree remaining-strings remaining-parse-tree)))

(defn full-parseo
  [combinator grammar strings parse-tree]
  (partial-parseo combinator grammar strings parse-tree () ()))

(defn instaparseo
  [instaparser strings parse-tree]
  (full-parseo (get (:grammar instaparser) (:start-production instaparser))
               (:grammar instaparser)
               strings parse-tree))

(defn generate-strings-for-parse-tree
  "Given a parser and a parse tree, returns the first n (or all) possible strings
that will return that parse tree. Could be infinite."
  [instaparser parse-tree & [n]]
  (let [grammar (:grammar instaparser)]
    (if-not n
      (run* [q]
            (full-parseo (get grammar (:start-production instaparser)) grammar q parse-tree))
      (run n [q]
           (full-parseo (get grammar (:start-production instaparser)) grammar q parse-tree)))))

(defn generate-possible-strings
  "Given a parser, returns the first n (or all) possible strings that can be parsed
by the parser. Could be infinite."
  [instaparser & [n]]
  (let [grammar (:grammar instaparser)]
    (if-not n
      (run* [q]
            (fresh [pt]
                  (full-parseo (get grammar (:start-production instaparser)) grammar q pt)))
      (run (or n 1) [q]
           (fresh [pt]
                  (full-parseo (get grammar (:start-production instaparser)) grammar q pt))))))