(ns dargwa-counter.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET]]
            [dargwa-counter.parser :as p]
            [dargwa-counter.tests :as dc-tests]
            [dargwa-counter.localstorage :as ls]
            [dargwa-counter.db :as db]))
;; -------------------------
;; Db
(def default-state {})
(defonce app (atom default-state))
(def app-key "app")

(defn save-app!
  []
  (ls/set-item! app-key @app)
  (js/alert "Saved!"))

(defn load-app!
  []
  ;;(.log js/console (clj->js (ls/get-item app-key)))
  (reset! app (ls/get-item app-key))
  (js/alert "Loaded!"))

(defn on-readed
  [e]
  (let [text (-> e .-target .-result)]
    (swap! app assoc
           :tales (p/extract-tales text)
           :current-tale-index 0)))

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

(defn set-current-tale-index
  [id]
  (swap! app assoc :current-tale-index id))
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
   {:style {:margin-top 0}}
   [:h1 "Dargwa language counter app"]
   [:p.pull-left [upload-btn file-name]]
   [:div.pull-right
    [:a.btn.btn-xs.btn-success
     {:on-click save-app!}
     "Save"]
    [:a.btn.btn-xs.btn-danger
     {:on-click load-app!
      :style {:margin-left "10px"}}
     "Load"]]
   [:div.clearfix]])

(defn debug-state
  []
  (let [dapp @app
        dbg (dissoc dapp :tales)]
    [:div
    #_[:pre
       {:style {:white-space "pre-wrap"}}
       (with-out-str (cljs.pprint/pprint (get (:tales @app) (:current-tale-index @app))))
       ]
    [:pre
     {:style {:white-space "pre-wrap"}}
     (with-out-str (cljs.pprint/pprint dbg))]]))

(defn tales-menu
  [tales current-tale-index]
  [:select.form-control
   {:on-change #(set-current-tale-index (-> % .-target .-value js/parseInt))
    :value current-tale-index}
   (for [{:keys [id name]} tales]
     ^{:key id}
     [:option {:value id} name])])

(defn tale-header-component
  [{:keys [name author text-lines]}]
  [:div
   [:h3 {:style {:margin-top 0}} (str name " (" (count text-lines) " предложений)")]
   [:h5 author]])

(defn words-component
  [words]
  [:table.table.table-bordered.table-words
   [:thead
    [:tr
     (for [{:keys [dargwa part-of-speech position]} words]
       ^{:key position}
       [:th
        {:class (when (= part-of-speech :predicate) "m-predicate")}
        dargwa]
       )]]
   [:tbody
    [:tr
     (for [{:keys [translation part-of-speech position]} words]
       ^{:key position}
       [:td
        {:class (when (= part-of-speech :predicate) "m-predicate")}
        translation]
       )]]])

(defn marks-component
  [marks]
  [:ul.b-marks
   (for [{:keys [pronoun referent]} marks]
     ^{:key (str pronoun referent)}
     [:li (str "(" pronoun ", " referent ")")])])

(defn tale-editor
  [{:keys [name author text-lines]}]
  [:table.table
   {:style {:table-layout "fixed"}}
   [:thead
    [:tr
     [:th 
      {:style {:width "200px"}}
      "Отметки"]
     [:th "Слова и полный перевод"]]]
   [:tbody
    (for [{:keys [id words marks translation]} text-lines]
      ^{:key id}
      [:tr
       [:td
        [marks-component marks]]
       [:td
        [:div
         [words-component words]
         translation]]])]])

(defn tales-component
  [app]
  (let [tales (db/list-tales app)
        current-tale (db/get-current-tale app)]
    (when (seq tales)
      [:div
       [:div.row
        [:div.col-md-4 [tales-menu tales (:current-tale-index app)]]
        [:div.col-md-8 [tale-header-component current-tale]]]
       [:hr]
       [tale-editor current-tale]])))

(defn home-page []
  [:div.container
   [header (:file-name @app)]
   [tales-component @app]
   [debug-state]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
