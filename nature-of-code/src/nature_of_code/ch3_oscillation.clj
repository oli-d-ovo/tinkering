(ns nature-of-code.ch3-oscillation
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [nature-of-code.domain.movers :as movers]))

(defn apply-if
  [pred f v]
  (if pred (f v) v))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)

  {:movers (repeatedly 2 movers/->mover)})

(defn update-state [{:keys [forces surfaces] :as state}]
  (-> state
      (update :movers #(map (movers/update-mover forces surfaces %) %))))

(defn draw-state [state]
  (q/background 240)

  (q/no-stroke)
  (q/fill 200)
  (q/stroke 20)
  (q/stroke-weight 3)

  (doseq [{:keys [location mass angular]} (:movers state)]
    (let [r (* mass 16)]
      (q/push-matrix)
      (apply q/translate location)
      (q/rotate (:angle angular))
      (q/rect 0 0 r r)
      (q/pop-matrix))))

(q/defsketch nature-of-code
  :title "You spin my circle right round"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode])
