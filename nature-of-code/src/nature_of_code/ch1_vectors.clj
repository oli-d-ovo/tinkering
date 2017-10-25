(ns nature-of-code.ch1-vectors
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [nature-of-code.domain.vectors :as v]
            [nature-of-code.util.keys :as k]))

(def initial-state
  {:color 0
   :time 0
   :movers []
   :mouse-position [0 0]})

(defn ->mover
  []
  {:location [(q/random (q/width)) (q/random (q/height))]
   :velocity [0 0]
   :acceleration [0 0]})

(defn update-mover
  [mouse-position]
  (fn [{:keys [location velocity acceleration] :as m}]
    (let [new-acceleration (-> (v/sub mouse-position location)
                               v/normalize
                               (v/mult 0.5))
          new-velocity (-> (v/add velocity new-acceleration)
                           (v/limit 10))
          new-location (v/add location new-velocity)]
      {:location new-location
       :velocity new-velocity
       :acceleration new-acceleration})))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  (assoc initial-state :movers (repeatedly 10 ->mover)))

(defn update-state [state]
  (let [perlin-time (:time state)
        mouse-position [(q/mouse-x) (q/mouse-y)]]
    (-> state
        (update :color #(mod (+ % 0.7) 255))
        (update :time + 0.01)
        (update :movers #(map (update-mover mouse-position) %))
        (assoc :centre [(/ (q/width) 2) (/ (q/height) 2)])
        (assoc :mouse-position mouse-position))))

(defn draw-state [state]
  (q/background 240)
  (q/stroke 20)
  (q/stroke-weight 3)

  (doseq [{:keys [location]} (:movers state)]
    (apply q/ellipse (concat location [16 16]))))

(comment
  (q/defsketch nature-of-code
    :title "You spin my circle right round"
    :size [500 500]
                                        ; setup function called only once, during sketch initialization.
    :setup setup
                                        ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
                                        ; This sketch uses functional-mode middleware.
                                        ; Check quil wiki for more info about middlewares and particularly
                                        ; fun-mode.
    :middleware [m/fun-mode]))
