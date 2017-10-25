(ns nature-of-code.util.keys
  (:require [quil.core :as q]))

(defn for-keys
  [default & ks]
  (or (->> ks
           (filter #(= (q/key-as-keyword) (key %)))
           vals
           first)
      default))
