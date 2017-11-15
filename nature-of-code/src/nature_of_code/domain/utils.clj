(ns nature-of-code.domain.utils)

(defn inside?
  [[x1 y1 x2 y2] [x y]]
  (and (< x1 x (+ x1 x2))
       (< y1 y (+ y1 y2))))

(defn constrain
  [x l u]
  (cond
    (< x l) l
    (> x u) u
    :else x))
