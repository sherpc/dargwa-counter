(ns dargwa-counter.parser
  (:require [clojure.string :as s]))

(defn clean-line
  [l]
  (-> l
      (s/replace #"\(\d+\)" "")
      s/trim))

(defn extract-pronoun
  [line]
  (let [cols (s/split line #"\t")]
    {:sentence (nth cols 0)
     :pronoun (nth cols 2 :error)
     :referent (nth cols 3 (seq line))
     }))

(defn only-not-blank
  [ss]
  (filter #(-> % s/trim s/blank? not) ss))

(defn is-line-marked?
  [line]
  (-> line
      (s/split #"\t")
      only-not-blank
      count
      (>= 3)))

(only-not-blank [" " "asd" "   "])

(defn parse-line
  [l]
  (let [line (clean-line l)]
    (if (is-line-marked? line)
      (extract-pronoun line)
      {:sentence (-> line (s/split #"\t") first s/trim)})))

(defn parse-text
  [text]
  (->> text
       s/split-lines
       (map parse-line)))

(defn ->to-tale
  [[name raw-author & lines]]
  {:name (s/trim name)
   :author (s/trim raw-author)
   :text-lines lines})

(defn extract-tales
  [text]
  (->> text
       s/split-lines
       (partition-by s/blank?)
       (remove #(every? s/blank? %))
       (map ->to-tale)
       (take 3)))
