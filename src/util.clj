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

(defn bounding-box [points]
  (let [[min-x max-x] (apply (juxt min max) (map first points))
        [min-y max-y] (apply (juxt min max) (map second points))]
    [[min-x min-y] [max-x max-y]]))
