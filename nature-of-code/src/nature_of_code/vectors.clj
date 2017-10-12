(ns nature-of-code.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0
   :mouse-position [0 0]})

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.1)
   :centre [(/ (q/width) 2) (/ (q/height) 2)]
   :mouse-position [(q/mouse-x) (q/mouse-y)]})

(defn draw-state [state]
  (q/background 240)
  (q/stroke 20)
  (q/stroke-weight 3)
  (q/translate (:centre state))
  (apply (partial q/line 0 0) (map - (:mouse-position state) (:centre state))))

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
