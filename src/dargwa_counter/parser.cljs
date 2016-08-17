(ns dargwa-counter.parser
  (:require [clojure.string :as s]
            [schema.core :as sc
             :include-macros true]
            [dargwa-counter.db :as db]))

(comment (sc/set-fn-validation! true))

;; Text

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

;; Tales

(defn ->to-tale
  [[name raw-author & lines]]
  {:name (s/trim name)
   :author (s/trim raw-author)
   :text-lines lines})

(defn extract-tales
  [text]
  (->> text
       s/split-lines
       (remove #(s/starts-with? % "["))
       (partition-by s/blank?)
       (remove #(every? s/blank? %))
       (map ->to-tale)
       ;;(take 3)
       (db/->ids-hash-map)))


;; Words

(def predicate-marks #{":pf" ":ipf"})

(defn part-of-speech
  [translation]
  (if (some #(s/includes? translation %) predicate-marks)
    :predicate
    :unknown))

(sc/defn word :- db/Word
  [dargwa translation pos sentence]
  {:dargwa dargwa
   :translation translation
   :position pos
   :part-of-speech (part-of-speech translation)
   :sentence-id sentence})
