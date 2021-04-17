(ns aoc.year2020.day10
  (:require [aoc.util :refer [input-lines memoize-rec]]))

(def data (map read-string (input-lines 2020 10)))

(def sample1 (->> (clojure.string/split-lines "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
                  (map read-string)))
(def sample2 (->> (clojure.string/split-lines "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")
                  (map read-string)))

(defn pt2 [data]
  (let [maximum      (+ (apply max data) 3)
        has-adapter? (conj (set data) maximum)
        z            (memoize-rec
                       (fn f [n]
                         (cond
                           (zero? n) 1
                           (has-adapter? n) (+ (f (- n 1)) (f (- n 2)) (f (- n 3)))
                           :else 0)))]
    (z maximum)))

(comment
  ;; pt1
  (let [maximum (apply max data)
        data    (sort (conj data 0 (+ maximum 3)))
        ]
    (->> data
         (partition 2 1)
         (map (fn [[a b]] (- b a)))
         (frequencies)))

  (pt2 data)
  )