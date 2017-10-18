(ns nature-of-code.domain.forces
  (:require [nature-of-code.domain.vectors :as v]))

(defn ->acceleration
  [force]
  force)

(defn- collide-scalar
  [s [min max]]
  (cond (> s max) [max -1]
        (< s min) [min -1]
        :else [s 1]))

(defn collide
  [[x y] x-bounds y-bounds]
  (->> [(collide-scalar x x-bounds)
        (collide-scalar y y-bounds)]
       (apply mapv vector)))
