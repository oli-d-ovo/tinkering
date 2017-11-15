(ns nature-of-code.domain.forces
  (:require [nature-of-code.domain.vectors :as v]))

(defn ->acceleration
  [force mass]
  (v/div force mass))

(defn- collide-scalar
  [s [min max]]
  (cond (> s max) [(- max s) -1]
        (< s min) [(- min s) -1]
        :else [0 1]))

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

(defn drag
  [velocity c]
  (let [speed (v/mag velocity)]
    (-> velocity
        (v/mult -1)
        v/normalize
        (v/mult (* c speed speed)))))
