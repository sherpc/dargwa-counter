(ns dargwa-counter.core
  (:require [reagent.core :as reagent :refer [atom]]
            [ajax.core :refer [GET]]
            [dargwa-counter.parser :as p]
            [dargwa-counter.tests :as dc-tests]
            [dargwa-counter.localstorage :as ls]
            [dargwa-counter.db :as db]))

;; css (hell :( never do it like this! )

(def default-margin "15px")

;; utils

(defn event-value
  [e]
  (-> e .-target .-value))

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
    (.info js/console "readed file")
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

(defn set-current-tale-index!
  [id]
  (swap! app db/set-current-tale-index id))

(defn select-word!
  [s-id w-id]
  (swap! app db/select-word s-id w-id))

(defn mark-pronoun!
  [selected-word]
  (swap! app db/mark-pronoun selected-word))

(defn unmark-pronoun!
  []
  (swap! app db/unmark-pronoun))

(defn add-referent!
  []
  (swap! app db/add-referent))

(defn change-pronoun-name!
  [new-name]
  (swap! app db/change-pronoun-name new-name))

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
    #_[:pre
     {:style {:white-space "pre-wrap"}}
     (str dbg)]]))

(defn tales-menu
  [tales current-tale-index]
  [:select.form-control
   {:style {:width "auto" :height "30px"}
    :on-change #(set-current-tale-index! (-> % .-target .-value js/parseInt))
    :value current-tale-index}
   (for [{:keys [id name problems]} tales
         :let [has-problems? (pos? problems)]]
     ^{:key id}
     [:option {:value id} (str name (when has-problems? " (!)"))])])

(defn tale-header-component
  [{:keys [name author problems pronouns]}]
  [:div
   [:h3 {:style {:margin-top 0}} name " " [:span.badge (count pronouns)]]
   [:h5 author]
   (when (> problems 0)
     [:div.alert.alert-warning.m-alert-without-bottom-margin
      (str "В тексте есть " problems " предложений без слов.")])])

(defn css-classes
  [preds-with-css]
  (->> preds-with-css
       (filter first)
       (map second)
       (clojure.string/join " ")))

(defn words-one-lang
  [{:keys [selected-word add-pronoun]} words s-id td-key w-key]
  [:tr
   (for [{:keys [part-of-speech id] :as w} words
         :let [text (w-key w)
               w-id {:s-id s-id :w-id id}
               is-selected? (= w-id selected-word)
               is-predicate? (= part-of-speech :predicate)
               is-pronoun? (= w-id (:pronoun add-pronoun))
               preds [[is-predicate? "m-word-predicate"]
                      [is-selected? "text-success bg-warning"]
                      [is-pronoun? "bg-success"]]
               css (css-classes preds)]]
     ^{:key id}
     [td-key
      {:class css
       :on-click #(select-word! s-id id)}
      text]
     )])

(defn words-component
  [ui words s-id]
  [:table.table.table-bordered.table-words
   [:thead
    [words-one-lang ui words s-id :th :dargwa]]
   [:tbody
    [words-one-lang ui words s-id :td :translation]]])

(defn mark-as-pronoun
  [selected-word]
  [:div.clearfix
   [:input.form-control.input-sm
    {:style {:margin-bottom default-margin}
     :on-change #(change-pronoun-name! (event-value %))
     :placeholder "pronoun name"}]
   [:a.btn.btn-sm.btn-default.pull-right
    {:on-click #(mark-pronoun! selected-word)}
    [:i.fa.fa-plus]
    " mark as pronoun"]])

(defn cancel-mark-ref-btn
  []
  [:a.btn.btn-sm.btn-default
   {:on-click unmark-pronoun!}
   [:i.fa.fa-times]
   " cancel"])

(defn mark-as-ref-btns
  []
  [:div.clearfix
   [:div.pull-left
    [cancel-mark-ref-btn]]
   [:div.pull-right
    [:a.btn.btn-sm.btn-success
     {:on-click add-referent!}
     [:i.fa.fa-check]
     " add referent"]]])

(defn pls-mark-ref
  []
  [:div
   [:p "now select a referent"]
   [cancel-mark-ref-btn]])

(defn error-ref-mark
  []
  [:div
   [:p.text-danger "can't select referent that is after pronoun"]
   [cancel-mark-ref-btn]])

(defn word-actions-bar
  [{:keys [selected-word add-pronoun]} s-id]
  (let [{:keys [pronoun]} add-pronoun
        s-is-pronoun? (= s-id (:s-id pronoun))]
    (if add-pronoun
      (cond
        (and s-is-pronoun? (= selected-word pronoun)) [error-ref-mark]
        (and s-is-pronoun? (nil? selected-word)) [pls-mark-ref]
        (= s-id (:s-id selected-word)) (if (db/w> pronoun selected-word)
                                         [mark-as-ref-btns]
                                         [error-ref-mark])
        )
      (when (= s-id (:s-id selected-word))
        [mark-as-pronoun selected-word]))))

(defn marks-component
  [ui marks s-id]
  [:div
   [word-actions-bar ui s-id]
   [:ul.b-marks
    (for [{:keys [id pronoun referent]} marks]
      ^{:key id}
      [:li (str "(" pronoun " / " referent ")")
       #_[:span
          [:a.btn.btn-xs.text-danger
           {:on-click #(remove-mark! s-id id)}
           [:i.fa.fa-times]]]])]])

(defn sentence-without-words
  [translation]
  [:div
   [:div.alert.alert-warning.m-alert-without-bottom-margin
    [:h6 "Почему-то нет слов в предложении! Проверьте текст. Перевод:"]
    [:p translation]
    [:h6 "Русские символы в переводе:"]
    [:p (clojure.string/join ", " (filter p/russian-letters translation))]]])

(defn sentence-component
  [ui words s-id translation]
  (if (seq words)
    [:div
     {:style {:overflow-x "scroll"}}
     [words-component ui words s-id]
     translation]
    [sentence-without-words translation]))

(defn tale-editor
  [ui {:keys [name author text-lines]}]
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
       [:td [marks-component ui (db/select marks) id]]
       [:td [sentence-component ui (db/select words) id translation]]])]])

(defn tales-component
  [ui tales current-tale ]
  (when (seq tales)
    [:div
     [tale-header-component current-tale]
     [:hr]
     [tale-editor ui current-tale]]))

(defn header
  [{:keys [file-name current-tale-index]} {:keys [total-problems] :as data} csv-export-uri tales]
  [:div.page-header
   {:style {:margin-top default-margin}}
   (if file-name
     [:div.pull-left
      [tales-menu tales current-tale-index]]
     [:div.pull-left
      {:style {:padding-top "5px"}}
      [upload-btn file-name]])
   (when (and total-problems (> total-problems 0))
     [:div.pull-left
      [:div.alert.alert-warning.m-alert-left-navbar
       (str total-problems " истории с ошибками!")]])
   [:div.pull-right
    (if (empty? data)
      [:a.btn.btn-sm.btn-warning
       {:on-click load-app!
        :style {:margin-left default-margin}}
       [:span.fa.fa-download]
       " Загрузить"]
      [:div 
       [:a.btn.btn-sm.btn-success
        {:href csv-export-uri
         :download "report.csv"}
        [:i.fa.fa-file-excel-o]
        " Отчет"]
       [:a.btn.btn-sm.btn-default
        {:on-click save-app!
         :style {:margin-left default-margin}}
        [:span.fa.fa-save]
        " Сохранить"]])]
   [:div.clearfix]])

(defn home-page []
  (let [{:keys [ui data csv-export-uri]} @app
        tales (-> data :tales db/select)
        current-tale (db/get-current-tale ui data)]
    [:div.container.m-card
     [header ui data csv-export-uri tales]
     [debug-state]
     [tales-component ui tales current-tale]]))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
