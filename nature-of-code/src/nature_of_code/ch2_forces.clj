(ns nature-of-code.ch2-forces
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [nature-of-code.domain.vectors :as v]
            [nature-of-code.domain.forces :as f]
            [nature-of-code.util.keys :as k]))

(def initial-state
  {:color 0
   :time 0
   :movers []
   :gravity [[0 0.2]]
   :mouse-position [0 0]})

(defn ->mover
  []
  {:location [(q/random (q/width)) 0]
   :mass (q/random 1 5)
   :velocity [0 0]
   :acceleration [0 0]})

(defn update-mover
  [[gravity & forces]]
  (fn [{:keys [location mass velocity acceleration] :as m}]
    (let [[coll-loc coll-vel] (f/collide location [0 (q/width)] [0 (q/height)])
          new-acceleration (->> (conj forces (f/friction velocity 0.1) (v/mult gravity mass))
                                (map #(f/->acceleration % mass))
                                (reduce v/add acceleration))
          new-velocity (-> (v/add velocity new-acceleration)
                           (v/multv coll-vel))
          new-location (v/add coll-loc new-velocity)]
      (assoc m :location new-location
               :velocity new-velocity
               :acceleration [0 0]))))

(defn additional-forces
  [perlin-time]
  [(if (q/mouse-pressed?) [0.5 0] [0 0])
   (v/mult [0 0] (q/noise perlin-time))])

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  (assoc initial-state :movers (repeatedly 20 ->mover)))

(defn update-state [state]
  (let [forces (concat (:gravity state) (additional-forces (:time state)))
        mouse-position [(q/mouse-x) (q/mouse-y)]]
    (-> state
        (update :color #(mod (+ % 0.7) 255))
        (update :time + 0.01)
        (update :movers #(map (update-mover forces) %))
        (assoc :centre [(/ (q/width) 2) (/ (q/height) 2)])
        (assoc :mouse-position mouse-position))))

(defn draw-state [state]
  (q/background 240)
  (q/stroke 20)
  (q/stroke-weight 3)

  (doseq [{:keys [location mass]} (:movers state)]
    (let [r (* mass 16)]
      (apply q/ellipse (concat location [r r])))))

(q/defsketch nature-of-code
  :title "You spin my circle right round"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top :no-bind-output]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
