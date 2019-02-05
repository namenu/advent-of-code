;; --- Day 6: Memory Reallocation ---
(ns year2017.day06
  (:require [util :refer [find-cycle]]))

(def banks [0 2 7 0])
(def banks [5 1 10 0 1 7 13 14 3 12 8 10 7 12 0 6])

(defn max-bank [banks]
  (reduce (fn [[mi mb] [i b]]
            (if (> b mb) [i b] [mi mb]))
          (map-indexed vector banks)))

(defn spread [banks]
  (let [[i blocks] (max-bank banks)
        iterator (->> (cycle (range (count banks)))
                      (drop (inc i))
                      (take blocks))]
    (reduce #(update %1 %2 inc)
            (assoc banks i 0)
            iterator)))

(let [[i j] (find-cycle (iterate spread banks))]
  [j (- j i)])