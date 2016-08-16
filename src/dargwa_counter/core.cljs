(ns dargwa-counter.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET]]
            [dargwa-counter.parser :as p]))
;; -------------------------
;; Db
(def default-state {})
(defonce app (atom default-state))

(defn on-readed
  [e]
  (let [text (-> e .-target .-result)]
    (swap! app assoc :tales (p/extract-tales text))))

(defn put-upload
  [e]
  (.info js/console "load file")
  (let [target (.-currentTarget e)
        file (-> target .-files (aget 0))
        reader (js/FileReader.)]
    (set! (.-onload reader) on-readed)
    (.readAsText reader file)
    (swap! app assoc :file-name (.-name file))))

(defn clean-file
  []
  (reset! app default-state))
;; -------------------------
;; Views

(defn upload-btn
  [file-name]
  [:span.upload-label
   [:label
    [:input.hidden
     {:type "file" :accept ".txt" :on-change put-upload}]
    [:i.fa.fa-upload.fa-lg]
    (str " " (or file-name "click here to upload txt..."))]
   (when file-name
     [:span
      " "
      [:i.fa.fa-times {:on-click clean-file}]])])

(defn header
  [file-name]
  [:div.page-header
   [:h1 "Dargwa language counter app"]
   [:p [upload-btn file-name]]])

(defn debug-state
  []
  [:pre
   {:style {:white-space "pre-wrap"}}
   (with-out-str (cljs.pprint/pprint @app))])

(defn home-page []
  [:div.container
   [header (:file-name @app)]
   [debug-state]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
