(ns nature-of-code.ch3-oscillation
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [nature-of-code.domain.movers :as movers]))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)

  {:movers (repeatedly 20 movers/->mover)
   :mouse-position []})

(defn update-state [{:keys [forces surfaces mouse-position] :as state}]
  (-> state
      (assoc :mouse-position [(q/mouse-x) (q/mouse-y)])
      (update :movers #(map (movers/update-mover forces surfaces % mouse-position) %))))

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
      (q/rect 0 0 r (/ r 2))
      (q/pop-matrix))))

(q/defsketch nature-of-code
  :title "You spin my circle right round"
  :size [1000 1000]
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top :no-bind-output]
  :middleware [m/fun-mode])
