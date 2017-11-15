(ns nature-of-code.domain.movers
  (:require [quil.core :as q]
            [nature-of-code.domain.vectors :as v]
            [nature-of-code.domain.forces :as f]
            [nature-of-code.domain.surfaces :as s]))

(defn ->mover
  []
  {:location [(q/random (q/width)) (q/random (q/height))]
   :mass (q/random 1 5)
   :velocity [0 0]
   :acceleration [0 0]
   :angular {:angle 0
             :velocity 0}})

(defn attract-mover
  [m ms]
  (->> ms
       (filter #(not= m %))
       (map #(f/attract m %))))

(defn- update-angular
  [{:keys [angle velocity] :as a} acceleration]
  (let [new-velocity (+ velocity acceleration)]
    (assoc a
           :velocity new-velocity
           :angle (+ angle new-velocity))))

(defn update-mover
  [{:keys [gravity additional]} surfaces movers]
  (fn [{:keys [location mass velocity acceleration angular] :as m}]
    (let [drag (s/drag-at location surfaces)
          attraction (attract-mover m movers)
          new-acceleration (->> (vector (v/mult gravity mass) (f/drag velocity drag) #_(v/sub location mouse-position))
                                (concat attraction)
                                (concat additional)
                                (map #(f/->acceleration % mass))
                                (reduce v/add acceleration))
          new-velocity (v/add velocity new-acceleration)
          new-location (v/add location new-velocity)]
      (assoc m :location new-location
               :velocity new-velocity
               :acceleration [0 0]
               :angular (update-angular angular (/ (first new-acceleration) 10))))))
