(ns nature-of-code.domain.vectors)

(defn sub
  [v1 v2]
  (map - v1 v2))

(defn add
  [v1 v2]
  (map + v1 v2))

(defn mult
  [v n]
  (map (partial * n) v))

(defn div
  [v n]
  (map #(/ % n) v))

(defn mag
  [v]
  (->> v
       (map #(* % %))
       (reduce +)
       Math/sqrt))

(defn normalize
  [v]
  (let [m (mag v)]
    (if (= m 0)
      v
      (div v m))))

(defn- flip-scalar
  [s min max]
  (cond (> s max) min
        (< s min) max
        :else s))

(defn flip
  [[x y] x-bounds y-bounds]
  [(apply (partial flip-scalar x) x-bounds)
   (apply (partial flip-scalar y) y-bounds)])
