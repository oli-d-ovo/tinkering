(ns nature-of-code.domain.vectors)

(def zero [0 0])

(defn- op
  [f x1 x2]
  (if x2 (map f x1 x2) x1))

(defn sub
  [v1 v2]
  (op - v1 v2))

(defn add
  [v1 v2]
  (op + v1 v2))

(defn multv
  [v1 v2]
  (op * v1 v2))

(defn divv
  [v1 v2]
  (op / v1 v2))

(defn mult
  [v n]
  (map #(* % (or n 1)) v))

(defn div
  [v n]
  (map #(/ % (or n 1)) v))

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

(defn limit
  [v l]
  (let [magnitude (mag v)]
    (if (< magnitude l)
      v
      (div v (/ magnitude l)))))
