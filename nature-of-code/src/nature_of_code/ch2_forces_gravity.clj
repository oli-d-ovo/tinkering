(ns nature-of-code.ch2-forces-gravity
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [nature-of-code.domain.vectors :as v]
            [nature-of-code.domain.forces :as f]
            [nature-of-code.domain.utils :as u]
            [nature-of-code.util.keys :as k]))

(def initial-state
  {:color 0
   :time 0
   :movers []
   :surfaces []
   :forces {:gravity [0 0]}
   :mouse-position [0 0]})

(defn ->mover
  []
  {:location [(q/random (q/width)) (q/random (q/height))]
   :mass (q/random 1 5)
   :velocity [0 0]
   :acceleration [0 0]})

(defn ->surface
  [x y width height drag]
  {:position [x y width height]
   :drag drag})

(defn drag-at
  [location surfaces]
  (->> surfaces
       (filter #(u/inside? (:position %) location))
       (map :drag)
       (reduce +)))

(defn attract-mover
  [m ms]
  (->> ms
       (filter #(not= m %))
       (map #(f/attract m %))))

(defn update-mover
  [{:keys [gravity additional]} surfaces movers]
  (fn [{:keys [location mass velocity acceleration] :as m}]
    (let [drag (drag-at location surfaces)
          attraction (attract-mover m movers)
          new-acceleration (->> (vector (v/mult gravity mass) (f/drag velocity drag))
                                (concat attraction)
                                (concat additional)
                                (map #(f/->acceleration % mass))
                                (reduce v/add acceleration))
          new-velocity (v/add velocity new-acceleration)
          new-location (v/add location new-velocity)]
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
  (assoc initial-state
         :movers (repeatedly 20 ->mover)))

(defn update-state [{:keys [forces surfaces] :as state}]
  (let [forces (-> forces
                   (assoc :additional (additional-forces (:time state))))
        mouse-position [(q/mouse-x) (q/mouse-y)]]
    (-> state
        (update :color #(mod (+ % 0.7) 255))
        (update :time + 0.01)
        (update :movers #(map (update-mover forces surfaces %) %))
        (assoc :centre [(/ (q/width) 2) (/ (q/height) 2)])
        (assoc :mouse-position mouse-position))))

(defn draw-state [state]
  (q/background 240)

  (q/no-stroke)
  (doseq [{:keys [position drag]} (:surfaces state)]
    (q/fill (* 255 drag))
    (apply q/rect position))

  (q/fill 200)
  (q/stroke 20)
  (q/stroke-weight 3)

  (doseq [{:keys [location mass angular]} (:movers state)]
    (let [r (* mass 16)]
      (q/push-matrix)
      (q/rotate (:angle angular))
      (apply q/ellipse (concat location [r r]))
      (q/pop-matrix))))

(comment
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
    :middleware [m/fun-mode]))
