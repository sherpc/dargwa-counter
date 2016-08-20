(ns dargwa-counter.parser
  (:require [clojure.string :as s]
            [schema.core :as sc
             :include-macros true]
            [dargwa-counter.db :as db]))

(def punctuation-marks #{"," "–" "-" "." ":" "…"})

(comment (sc/set-fn-validation! true))

;; Text

(defn clean-line
  [l]
  (-> l
      (s/replace #"\(\d+\)" "")
      (s/replace #"^\d+?\." "")
      s/trim))

(defn extract-pronoun
  [line]
  (let [cols (s/split line #"\t")
        pronoun (nth cols 2 :error)
        referent (nth cols 3 nil)
        is-not-full? (or (s/blank? pronoun) (s/blank? referent))]
    {:sentence (nth cols 0)
     :mark (when-not is-not-full? {:pronoun pronoun :referent referent})
     }))

(defn only-not-blank
  [ss]
  (filter #(-> % s/trim s/blank? not) ss))

(defn is-line-marked?
  [line]
  (-> line
      (s/split #"\t")
      only-not-blank
      count
      (>= 3)))

(defn parse-line
  [l]
  (let [line (clean-line l)]
    (if (is-line-marked? line)
      (extract-pronoun line)
      {:sentence (-> line (s/split #"\t") first s/trim)})))

;; Words

(def predicate-marks #{":pf" ":ipf"})

(defn part-of-speech
  [translation]
  (if (some #(s/includes? translation %) predicate-marks)
    :predicate
    :unknown))

(defn word
  [dargwa translation]
  {:dargwa dargwa
   :translation translation
   :part-of-speech (part-of-speech translation)})

;; Sentences

(defn char->int
  [c]
  (.charCodeAt c 0))

(defn char-range
  [start end]
  (map char (range (char->int start) (inc (char->int end)))))

(def russian-lower-letters (char-range \а \я))
(def russian-upper-letters (char-range \А \Я))
(def russian-letters (set (concat russian-lower-letters russian-upper-letters)))

(defn is-dargwa?
  [s]
  (not (some russian-letters s)))

(defn split-to-words
  [st]
  (s/split st #" "))

(defn fix-translation-reducer
  [result w]
  (let [last-w (peek result)]
    (if (and last-w (s/ends-with? last-w ":"))
      (-> result pop (conj (str last-w w)))
      (conj result w))))

(defn fix-translation
  [r-words]
  (reduce fix-translation-reducer [] r-words))


(defn clean-dargwa-special-symbols
  [dargwa]
  (-> dargwa
      (s/replace #"\(?0[\w+]{0,3}-[erg|abs|dat]{3}\)?" "")
      (s/replace #"subj\(\w{0,2}\)" "")
      (s/replace #"\(\.{3}\)" "")
      (s/replace #"\[*" "")
      (s/replace #"\]*" "")))

(defn clean-russian-special-symbols
  [russian]
  (-> russian
      (s/replace #"\(рус\.\)" "")))

(defn split-sentence-to-words
  [dargwa russian]
  (let [d-words (->>
                 dargwa
                 clean-dargwa-special-symbols
                 split-to-words
                 (map s/trim)
                 (remove s/blank?)
                 (remove punctuation-marks))
        r-words (->>
                 russian
                 clean-russian-special-symbols
                 split-to-words
                 (remove s/blank?)
                 fix-translation)
        d-c (count d-words)
        r-c (count r-words)
        ids (range d-c)]
    (if (not= d-c r-c)
      ;;[]
      (do
        (.log js/console dargwa)
        (.log js/console russian)
        (.log js/console (clj->js d-words))
        (.log js/console (clj->js r-words))
        (throw (str "In sentence and translation different words count." )))
      (map word d-words r-words))))

(defn concat-words
  [words]
  (->> words
       (apply concat)
       db/->ids-hash-map))

(defn index-marks
  [marks]
  (->> marks
       (remove nil?)
       db/->ids-hash-map))

(defn make-sentence
  [ln {:keys [words marks]} translation]
  {:id ln
   :words (concat-words words)
   :marks (index-marks marks)
   :translation translation})

(defn add-line
  [{:keys [current result] :as state} line]
  (let [{:keys [ln]} current
        pl (parse-line line)
        sentence (:sentence pl)
        dargwa? (is-dargwa? sentence)]
    ;; Если даргинский, просто добавим предложение в текущее состояние
    (if dargwa?
      (-> state
          (assoc-in [:current :dargwa] sentence)
          (update-in [:current :marks] conj (:mark pl)))
      ;; Иначе нам надо посмотреть, заполнена даргинская часть, или нет
      ;; Если да -- делаем из неё и текущей русской слова
      ;; Если нет -- значит записываем литературный перевод и отправляем предложение в result
      (if (:dargwa current)
        (-> state
            (assoc-in [:current :dargwa] nil)
            (update-in [:current :words] conj (split-sentence-to-words (:dargwa current) sentence)))
        (-> state
            (assoc :current {:ln (inc ln) :words [] :marks []})
            (update-in [:result] conj (make-sentence ln current sentence)))))))

(def initial-state {:current {:ln 0 :words [] :marks []} :result []})

(defn parse-text
  [text-lines]
  (:result (reduce add-line initial-state text-lines)))

;; Tales

(defn ->to-tale
  [[name raw-author & lines]]
  (.info js/console (str "Parsing tale '" (s/trim name) "'..."))
  {:name (s/trim name)
   :author (s/trim raw-author)
   :text-lines (-> lines parse-text db/->ids-hash-map)})

(defn extract-tales
  [text]
  (->> text
       s/split-lines
       ;;(remove #(s/starts-with? % "["))
       (partition-by s/blank?)
       (remove #(every? s/blank? %))
       (map ->to-tale)
       (drop 0)
       (take 20)
       (db/->ids-hash-map)))


