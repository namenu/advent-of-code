(ns util)

(defn first-duplicate-key [key-fn xs]
  (let [result (reduce (fn [seen x]
                         (let [k (key-fn x)]
                           (if (seen k)
                             (reduced x)
                             (conj seen k))))
                       #{} xs)]
    (if (set? result)
      nil
      result)))

(defn fixed-point [f x]
  (reduce #(if (= %1 %2) (reduced %1) %2)
          (iterate f x)))

(defn bounding-box [points]
  (let [[min-x max-x] (apply (juxt min max) (map first points))
        [min-y max-y] (apply (juxt min max) (map second points))]
    [[min-x min-y] [max-x max-y]]))

(defn range-incl
  ([end]
   (range (inc end)))
  ([start end]
   (range start (inc end)))
  ([start end step]
   (range start (inc end) step)))

(defn manhattan-dist [a b]
  (reduce + (map #(Math/abs ^Integer (- %1 %2)) a b)))
