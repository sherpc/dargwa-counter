(ns dargwa-counter.db
  (:require [schema.core :as sc
             :include-macros true]))

(def default-ui {:file-name nil
                 :current-tale-index 0
                 :selected-word nil
                 :add-mark-popup nil})

(def default-app {:ui default-ui :data {}})

(defn ->ids-hash-map
  [xs]
  (->> xs
       (map-indexed (fn [i x] [i (assoc x :id i)]))
       (into {})))

(defn select
  [indexed-seq]
  (->> indexed-seq
       (map (fn [[_ v]] v))
       (sort-by :id)))

(defn get-current-tale
  [{:keys [current-tale-index]} {:keys [tales]}]
  (-> tales
      (get current-tale-index)
      (update :text-lines select)))

(defn update-in-tale
  [{:keys [current-tale-index] :as db} path f arg1]
  (update-in db (concat [:tales current-tale-index] path) f arg1))

;; ui
;; first arg -- app

(defn show-add-mark-popup
  [app s-id]
  (assoc-in app [:ui :add-mark-popup] {:s-id s-id}))

(defn close-add-mark-popup
  [app]
  (assoc-in app [:ui :add-mark-popup] nil))

(defn change-add-mark-text
  [app k v]
  (assoc-in app [:ui :add-mark-popup k] v))

(defn add-mark
  [{:keys [ui data] :as app}]
  (let [{:keys [s-id pronoun referent]} (:add-mark-popup ui)
        current-tale-index (:current-tale-index ui)
        marks (get-in data [:tales current-tale-index :text-lines s-id :marks])
        last-id (->> marks (map (fn [[id _]] id)) (apply max))
        new-id (inc (or last-id -1))
        new-mark {:pronoun pronoun :referent referent :id new-id}]
    (-> app
        (assoc-in [:data :tales current-tale-index :text-lines s-id :marks new-id] new-mark)
        close-add-mark-popup)))

(defn remove-mark
  [{:keys [ui] :as app} s-id m-id]
  (update-in app [:data :tales (:current-tale-index ui) :text-lines s-id :marks] dissoc m-id))

(defn select-word
  [app s-id w-id]
  (assoc-in app [:ui :selected-word] {:s-id s-id :w-id w-id}))

(defn clear-word-selection
  [app]
  (assoc-in app [:ui :selected-word] nil))

(defn mark-pronoun
  [app selected-word]
  (-> app
      clear-word-selection
      (assoc-in [:ui :add-pronoun] {:pronoun selected-word})))

(defn unmark-pronoun
  [app]
  (-> app
      clear-word-selection
      (assoc-in [:ui :add-pronoun] nil)))

(defn w>
  [{s1 :s-id w1 :w-id} {s2 :s-id w2 :w-id}]
  (case (compare s1 s2)
    -1 false
    0 (> w1 w2)
    1 true))
