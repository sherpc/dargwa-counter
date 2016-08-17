(ns dargwa-counter.tests
  (:require [dargwa-counter.parser :as p]
            [dargwa-counter.db :as db]
            [schema.core :as sc
             :include-macros true]))

(defn log [x] (.log js/console (clj->js x)))

(defn make-word
  [dargwa translation]
  (sc/with-fn-validation (p/word dargwa translation 0 0)))

(assert (= :unknown (-> (make-word "he.l-a-la" "этот-obl-gen") :part-of-speech)))
(assert (= :predicate (-> (make-word "kalg-un-ne=sa≡r" "down+оставаться:pf-pret-conv=cop≡f") :part-of-speech)))
(assert (= :predicate (-> (make-word "kalg-un-ne=sa≡r" "down+оставаться:ipf-pret-conv=cop≡f") :part-of-speech)))

(.info js/console "All tests passed.")

