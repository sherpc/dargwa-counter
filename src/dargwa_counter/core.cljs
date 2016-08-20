(ns dargwa-counter.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET]]
            [dargwa-counter.parser :as p]
            [dargwa-counter.tests :as dc-tests]
            [dargwa-counter.localstorage :as ls]
            [dargwa-counter.db :as db]))

;; css (hell :( never do it like this! )

(def default-margin "15px")

;; -------------------------
;; Db
(defonce app (atom db/default-app))
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
    (swap! app assoc :data (p/parse-file text))))

(defn put-upload
  [e]
  (.info js/console "load file")
  (let [target (.-currentTarget e)
        file (-> target .-files (aget 0))
        reader (js/FileReader.)]
    (set! (.-onload reader) on-readed)
    (.readAsText reader file)
    (swap! app assoc-in [:ui :file-name] (.-name file))))

(defn clean-file
  []
  (reset! app db/default-app))

(defn set-current-tale-index
  [id]
  (swap! app assoc-in [:ui :current-tale-index] id))
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

(defn debug-state
  []
  (let [dapp @app
        dbg (assoc-in dapp [:data :tales] "truncated")]
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
   {:style {:width "auto" :height "30px"}
    :on-change #(set-current-tale-index (-> % .-target .-value js/parseInt))
    :value current-tale-index}
   (for [{:keys [id name problems]} tales
         :let [has-problems? (pos? problems)]]
     ^{:key id}
     [:option {:value id} (str name (when has-problems? " (!)"))])])

(defn tale-header-component
  [{:keys [name author problems]}]
  [:div
   [:h3 {:style {:margin-top 0}} name]
   [:h5 author]
   (when (> problems 0)
     [:div.alert.alert-warning.m-alert-without-bottom-margin
      (str "В тексте есть " problems " предложений без слов.")])])

(defn words-component
  [words]
  [:table.table.table-bordered.table-words
   [:thead
    [:tr
     (for [{:keys [dargwa part-of-speech id]} words]
       ^{:key id}
       [:th
        {:class (when (= part-of-speech :predicate) "m-predicate")}
        dargwa]
       )]]
   [:tbody
    [:tr
     (for [{:keys [translation part-of-speech id]} words]
       ^{:key id}
       [:td
        {:class (when (= part-of-speech :predicate) "m-predicate")}
        translation]
       )]]])

(defn marks-component
  [marks]
  [:ul.b-marks
   (for [{:keys [id pronoun referent]} marks]
     ^{:key id}
     [:li (str "(" pronoun ", " referent ")")])])

(defn sentence-without-words
  [translation]
  [:div
   [:div.alert.alert-warning.m-alert-without-bottom-margin
    [:h6 "Почему-то нет слов в предложении! Проверьте текст. Перевод:"]
    [:p translation]
    [:h6 "Русские символы в переводе:"]
    [:p (clojure.string/join ", " (filter p/russian-letters translation))]]])

(defn sentence-component
  [words translation]
  (if (seq words)
    [:div
     {:style {:overflow-x "scroll"}}
     [words-component words]
     translation]
    [sentence-without-words translation]))

(defn tale-editor
  [{:keys [name author text-lines]}]
  [:table.table
   {:style {:table-layout "fixed"}}
   [:thead
    [:tr
     [:th {:style {:width "200px"}} "Отметки"]
     [:th "Слова и полный перевод"]]]
   [:tbody
    (for [{:keys [id words marks translation]} text-lines]
      ^{:key id}
      [:tr
       [:td [marks-component (db/select marks)]]
       [:td [sentence-component (db/select words) translation]]])]])

(defn tales-component
  [tales current-tale]
  (when (seq tales)
    [:div
     [tale-header-component current-tale]
     [:hr]
     [tale-editor current-tale]]))

(defn header
  [{:keys [file-name current-tale-index]} {:keys [total-problems]} tales]
  [:div.page-header
   {:style {:margin-top default-margin}}
   (if file-name
     [:div.pull-left
      [tales-menu tales current-tale-index]]
     [:div.pull-left
      {:style {:padding-top "5px"}}
      [upload-btn file-name]])
   (when total-problems
     [:div.pull-left
      [:div.alert.alert-warning.m-alert-left-navbar
       (str total-problems " истории с ошибками!")]])
   [:div.pull-right
    [:a.btn.btn-sm.btn-default
     {:on-click save-app!}
     [:span.fa.fa-save]
     " Сохранить"]
    [:a.btn.btn-sm.btn-warning
     {:on-click load-app!
      :style {:margin-left default-margin}}
     [:span.fa.fa-download]
     " Загрузить"]]
   [:div.clearfix]])

(defn home-page []
  (let [{:keys [ui data]} @app
        tales (-> data :tales db/select)
        current-tale (db/get-current-tale ui data)]
    [:div.container.m-card
     [header ui data tales]
     [debug-state]
     [tales-component tales current-tale]]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
