(ns dargwa-counter.db
  (:require [schema.core :as sc
             :include-macros true]))

(def default-ui {:file-name nil :current-tale-index 0})
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

(defn add-mark
  [{:keys [current-tale-index] :as db} s-id mark]
  (update-in db [:tales current-tale-index :text-lines s-id :marks] conj mark))
