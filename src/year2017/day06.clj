;; --- Day 6: Memory Reallocation ---
(ns year2017.day06
  (:require [util :refer [find-first first-duplicate]]))

(def banks [0 2 7 0])
(def banks [5 1 10 0 1 7 13 14 3 12 8 10 7 12 0 6])

(defn max-bank [banks]
  (reduce (fn [[mi mb] [i b]]
            (if (> b mb) [i b] [mi mb]))
          (map-indexed vector banks)))

(defn spread [banks]
  (let [[i blocks] (max-bank banks)
        iterator (cycle (range (count banks)))]
    (reduce
      #(update %1 %2 inc)
      (assoc banks i 0)
      (take blocks (drop (inc i) iterator)))))

(let [indexed-cycles (->> (iterate spread banks)
                          (map-indexed vector))
      [index pattern] (->> indexed-cycles
                           (first-duplicate second))
      [first-seen _] (find-first #(= (second %) pattern) indexed-cycles)]
  [index, (- index first-seen)])
