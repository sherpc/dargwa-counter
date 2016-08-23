(ns dargwa-counter.db
  (:require [schema.core :as sc
             :include-macros true]
            [dargwa-counter.localstorage :as ls]))

(def default-ui {:file-name nil
                 :current-tale-index 0
                 :selected-word nil
                 :add-mark-popup nil})

(def default-app {:ui default-ui :data {} :stats []})

(defn w>
  [{s1 :s-id w1 :w-id} {s2 :s-id w2 :w-id}]
  (case (compare s1 s2)
    -1 false
    0 (> w1 w2)
    1 true))

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

(defn select-word
  [app s-id w-id]
  (assoc-in app [:ui :selected-word] {:s-id s-id :w-id w-id}))

(defn clear-word-selection
  [app]
  (assoc-in app [:ui :selected-word] nil))

(defn set-current-tale-index
  [app id]
  (-> app
      clear-word-selection
      (assoc-in [:ui :current-tale-index] id)))

(defn change-pronoun-name
  [app new-name]
  (assoc-in app [:ui :pronoun-name] new-name))

(defn clear-pronoun-name
  [app]
  (change-pronoun-name app nil))

(defn mark-pronoun
  [app selected-word]
  (-> app
      clear-word-selection
      (assoc-in [:ui :add-pronoun] {:pronoun selected-word})))

(defn unmark-pronoun
  [app]
  (-> app
      clear-word-selection
      clear-pronoun-name
      (assoc-in [:ui :add-pronoun] nil)))

(defn add-stats
  [app tale new-p]
  (let [stat {:tale-name (:name tale)
              :pronoun-name (:name new-p)
              :distance (:distance new-p)}]
    (-> app
        (update-in [:stats] conj stat))))

(defn words-between
  [lines {s-start :s-id w-start :w-id} {s-end :s-id w-end :w-id}]
  (if (= s-start s-end)
    (->> s-start
         (nth lines)
         :words
         select
         (filter #(< w-start (:id %) w-end)))
    (let [from-lines-between (->> lines
                                  (filter #(< s-start (:id %) s-end))
                                  (mapcat #(-> % :words select)))
          ;;_ (.log js/console (clj->js from-lines-between))
          from-start-line (->> s-start
                               (nth lines)
                               :words
                               select
                               (filter #(-> % :id (> w-start))))
          ;;_ (.log js/console (clj->js from-start-line))
          from-end-line (->> s-end
                             (nth lines)
                             :words
                             select
                             (filter #(-> % :id (< w-end))))
          ;;_ (.log js/console (clj->js from-end-line))
          ]
      (concat from-start-line from-lines-between from-end-line))))

(defn predicates-between
  [lines pronoun referent]
  (let [words (words-between lines referent pronoun)]
    (->> words
         (map :part-of-speech)
         (filter #(= % :predicate))
         count)))

(def app-key "app")

(defn save-app!
  [app]
  (ls/set-item! app-key app)
  app)

(defn get-csv-uri
  [stats]
  (->> stats
       (map #(->> % select (clojure.string/join "\t")))
       (clojure.string/join "\n")
       (str "Tale\tPronoun\tDistance\n")
       (str "data:text/csv;charset=utf-8,")
       js/encodeURI))

(defn update-csv-export
  [app]
  (assoc-in app [:csv-export-uri] (get-csv-uri (:stats app))))

(defn add-referent
  [{:keys [ui data] :as app}]
  (let [pronoun-id (-> ui :add-pronoun :pronoun)
        referent-id (-> ui :selected-word)
        current-tale-index (:current-tale-index ui)
        tale (get-in data [:tales current-tale-index])
        pronouns (:pronouns tale)
        last-id (->> pronouns (map (fn [[id _]] id)) (apply max))
        new-id (inc (or last-id -1))
        distance (predicates-between (select (:text-lines tale)) pronoun-id referent-id)
        new-p {:id new-id
               :pronoun pronoun-id
               :referent referent-id
               :name (:pronoun-name ui)
               :distance distance}]
    (-> app
        (assoc-in [:data :tales current-tale-index :pronouns new-id] new-p)
        (add-stats tale new-p)
        unmark-pronoun
        update-csv-export
        save-app!)))

