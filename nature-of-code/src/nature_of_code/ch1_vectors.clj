(ns nature-of-code.ch1-vectors
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [nature-of-code.domain.vectors :as v]))

(def initial-state
  {:color 0
   :angle 0
   :movers []
   :mouse-position [0 0]})

(defn ->mover
  ([]
   (->mover 2))
  ([v]
   {:location [(q/random (q/width)) (q/random (q/height))]
    :velocity [(q/random (- v) v) (q/random (- v) v)]}))

(defn update-mover
  [{:keys [location velocity acceleration] :as m}]
  (let [new-velocity (v/add velocity acceleration)
        new-location (-> (v/add location new-velocity)
                         (v/flip [0 (q/width)]
                                 [0 (q/height)]))]
    (assoc m :location new-location)))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  (assoc initial-state :movers (repeatedly 5 #(->mover 5))))

(defn update-state [state]
  (-> state
      (update :color #(mod (+ % 0.7) 255))
      (update :angle #(+ % 0.1))
      (update :movers #(map update-mover %))
      (assoc :centre [(/ (q/width) 2) (/ (q/height) 2)])
      (assoc :mouse-position [(q/mouse-x) (q/mouse-y)])))

(defn draw-state [state]
  (q/background 240)
  (q/stroke 20)
  (q/stroke-weight 3)

  (doseq [{:keys [location]} (:movers state)]
    (apply q/ellipse (concat location [16 16]))))

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
  :middleware [m/fun-mode])
