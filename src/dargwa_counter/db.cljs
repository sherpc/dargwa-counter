(ns dargwa-counter.db
  (:require [schema.core :as sc
             :include-macros true]))

(def default-ui {:file-name nil
                 :current-tale-index 0
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
        new-id (inc last-id)
        new-mark {:pronoun pronoun :referent referent :id new-id}]
    (-> app
        (assoc-in [:data :tales current-tale-index :text-lines s-id :marks new-id] new-mark)
        close-add-mark-popup)))
