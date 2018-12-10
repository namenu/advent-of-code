(ns util)



(defn bounding-box [points]
  (let [[min-x max-x] (apply (juxt min max) (map first points))
        [min-y max-y] (apply (juxt min max) (map second points))]
    [[min-x min-y] [max-x max-y]]))
