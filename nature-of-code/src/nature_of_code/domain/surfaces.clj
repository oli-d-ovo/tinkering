(ns nature-of-code.domain.surfaces
  (:require [nature-of-code.domain.utils :as u]))

(defn ->surface
  [x y width height drag]
  {:position [x y width height]
   :drag drag})

(defn drag-at
  [location surfaces]
  (->> surfaces
       (filter #(u/inside? (:position %) location))
       (map :drag)
       (reduce +)))
