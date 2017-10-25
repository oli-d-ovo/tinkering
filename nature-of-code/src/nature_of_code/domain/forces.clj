(ns nature-of-code.domain.forces
  (:require [nature-of-code.domain.vectors :as v]))

(defn ->acceleration
  [force mass]
  (v/div force mass))

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

(defn friction
  [velocity magnitude]
  (-> velocity
      (v/mult -1)
      v/normalize
      (v/mult magnitude)))
