(ns dargwa-counter.db)

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
