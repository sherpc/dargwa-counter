(ns dargwa-counter.tests
  (:require [dargwa-counter.parser :as p]
            [dargwa-counter.db :as db]
            [clojure.string :as s]
            [schema.core :as sc
             :include-macros true]))

(defn log [x] (.log js/console (clj->js x)))

(defn make-word
  [dargwa translation]
  (p/word dargwa translation))


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

(def bug-text "(1)[subj(m) [ca-jna malla nasratːin qʼaqʼa-le-ħe-r-ka qʼ˳-an-ne le≡w-le],	 	ne	 		две иг		n1	
один-mult Мулла Насреддин улица-obl-in-el-down идти:ipf-prs-adv exst≡m-conv	 	 	 					
 ca admi [0m-abs 0n-erg hitːi-w≡aʁ-ib-le [0m-abs w≡irχu-le=wan]]	 	le	 				0.1	
один человек <догнать>ls-m≡гнать:pf-pret-adv m≡становиться:ipf=like	 	 	 				n2	
 q˳aˁntʼaˁ b≡aˁq-ib-le]=sa≡j malla nasratːin-li-ž]	 	 	 					
затылок:loc(lat) n≡ударять:pf-pret-adv=cop≡m Мулла Насреддин-obl-dat	 	 	 					
Однажды Мулла Насреддин шел по улице, и тут его догнал какой-то человек и изо всех сил ударил по затылку.	 	 	 					
(2) malla nasratːin simi-w≡ač’-ib-le], [ʕaˁsi-w≡iχ-ub-le]	 	 	 				n/0	
Мулла Насреддин <сердиться>ls-m≡lv:pf-pret-adv <гневаться>ls-m≡lv:pf-pret-adv	 	 	 				n/0	
[ča-r-ħela-w≡iž-ib-le]=sa≡j:	 	 	 				n/0	
<обернуться>on+up-el-behind-m≡lv-pret-adv=cop≡m	 	 	 					
 sun-ni=ra b≡irq-i-d ible	 	 	 					
сам-erg=add n≡ударять:ipf-th-1 cit	 	 	 					
Мулла Насреддин рассердился, разгневался, обернулся и подумал: «Я и сам (его) ударю!» 	 	 	 					
")

(def bug-text-2 "(2) malla nasratːin simi-w≡ač’-ib-le], [ʕaˁsi-w≡iχ-ub-le]	 	 	 				n/0	
  Мулла Насреддин <сердиться>ls-m≡lv:pf-pret-adv <гневаться>ls-m≡lv:pf-pret-adv	 	 	 				n/0	
  Мулла Насреддин рассердился, разгневался, обернулся и подумал: «Я и сам (его) ударю!» 	 	 	 					
")

(def bug-text-3 "(9) [[nasratːin-ni [(0m-abs) haj-w≡arqʼ-ib-le], 0m-abs uč-ib-le]=sa≡j] qʼadi-šːu	 		 				0.1	
Насреддин-erg <идти.вместе>ls-m≡lv:pf-pret-conv (m)вести:pf-pret-conv=cop≡m кади-ad(lat)	 		 				0.2	
Насреддин пошел (с ним), повел (его) к кади.	 	 	 					
")

;; 0{1:3}-[abs|erg|dat]

;;(log (p/parse-text (s/split-lines bug-text-3)))

(.info js/console "All tests passed.")

