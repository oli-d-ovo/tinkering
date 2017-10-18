(ns nature-of-code.domain.keys
  (:require [quil.core :as q]))

(defn for-keys
  [ks default]
  (or (->> ks
           (filter #(= (q/key-as-keyword) (key %)))
           vals
           first)
      default))
