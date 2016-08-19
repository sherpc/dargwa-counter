(ns dargwa-counter.tests
  (:require [dargwa-counter.parser :as p]
            [dargwa-counter.db :as db]
            [clojure.string :as s]
            [schema.core :as sc
             :include-macros true]))

(defn log [x] (.log js/console (clj->js x)))

(defn make-word
  [dargwa translation]
  (p/word dargwa translation 0))


(assert (= :unknown (-> (make-word "he.l-a-la" "этот-obl-gen") :part-of-speech)))
(assert (= :predicate (-> (make-word "kalg-un-ne=sa≡r" "down+оставаться:pf-pret-conv=cop≡f") :part-of-speech)))
(assert (= :predicate (-> (make-word "kalg-un-ne=sa≡r" "down+оставаться:ipf-pret-conv=cop≡f") :part-of-speech)))

;; Testing text lines reducing

(def input-text "(15) ʕaˁt čina-la b≡ačʼ-ib-se, – r≡ikʼ-u-l=da, –	 	 	 					
  ты:dat где-gen n≡прийти:pf-pret-atr f≡говорить:ipf-prs-conv=1	 	 	 					
 ʕeˁla arc=ra=ak:˳-ar-le, selle asː-ib-se=de hi.l?	этот	хил	игрушка	иг	смена			
ты:gen деньги=add≡neg.cop-th-conv как брать:pf-pret-atr=2sg этот	 	 	 					
«Откуда она (игрушка) у тебя взялась? – говорю. – У тебя и денег-то нет, как ты ее купила?»	 	 	 					")

(def input-lines (s/split-lines input-text))

(assert (p/is-dargwa? (first input-lines)))
(assert (not (p/is-dargwa? (second input-lines))))

(def parsed (p/add-line p/initial-state (first input-lines)))

;;(log parsed)
;;(log (p/add-line parsed (nth input-lines 1)))
;;(log (p/parse-text input-lines))

(def bug-text "(8) b≡uc-a, – r≡ikʼ-u-le=sa≡r, – ʕaˁcːe hi.ž	этот	хиз	абаз	иг	эмф	отдать		
n=хватать:pf-imp f≡говорить:ipf-prs-conv=cop≡f ты: inter(lat) этот	 	 	 					
«Возьми, — говорит, — его себе».	 	")

(log (p/parse-text (s/split-lines bug-text)))

(.info js/console "All tests passed.")

