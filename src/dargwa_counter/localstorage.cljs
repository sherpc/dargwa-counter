(ns dargwa-counter.localstorage
  (:require [cljs.reader :as edn]))

(def to-edn pr-str)

(def from-edn edn/read-string)

(defn set-item!
  "Set `key' in browser's localStorage to `val`."
  [key val]
  (.setItem (.-localStorage js/window) key (to-edn val)))

(defn get-item
  "Returns value of `key' from browser's localStorage."
  [key]
  (from-edn (.getItem (.-localStorage js/window) key)))

(defn remove-item!
  "Remove the browser's localStorage value for the given `key`"
  [key]
  (.removeItem (.-localStorage js/window) key))
