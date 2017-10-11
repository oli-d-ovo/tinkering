(ns nature-of-code.ball
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn apply-if
  [pred f v]
  (if pred (f v) v))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)

  {:position [100 100]
   :speed [1 3.3]})

(defn update-state [{[x y :as pos] :position
                     [x-vel y-vel] :speed}]
  (let [speed [(apply-if (not (< 0 x (q/width))) - x-vel)
               (apply-if (not (< 0 y (q/height))) - y-vel)]]
    {:position (map + pos speed)
     :speed speed}))

(defn draw-state [{[x y] :position}]
  (q/background 255)
  (q/stroke 0)
  (q/fill 175)
  (q/ellipse x y 16 16))

(q/defsketch bouncing-ball
  :title "You spin my circle right round"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
