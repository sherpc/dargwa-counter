(ns dargwa-counter.db
  (:require [schema.core :as sc
             :include-macros true]))

(defn ->ids-hash-map
  [xs]
  (->> xs
       (map-indexed (fn [i x] [i (assoc x :id i)]))
       (into {})))

(defn list-tales
  [{:keys [tales]}]
  (map (fn [[_ v]] v) tales))

(defn get-current-tale
  [{:keys [tales current-tale-index]}]
  (get tales current-tale-index))

(def Word
  {:dargwa sc/Str
   :translation sc/Str
   :part-of-speech (sc/enum :predicate :unknown)
   :position sc/Int
   :sentence-id sc/Int})

(def Sentence
  {:id sc/Int
   (sc/optional-key :pronoun) sc/Str
   (sc/optional-key :referent) sc/Str
   })

(def Text
  {:words [Word]
   :sentences [Sentence]})
