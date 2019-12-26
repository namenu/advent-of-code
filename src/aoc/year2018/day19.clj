(ns aoc.year2018.day19
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [aoc.util :refer [fixed-point]]
            [aoc.year2018.device :refer :all]))


(def input "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5")
(def input (->> (-> "year2018/day19.in" io/resource slurp)))

;part1
(-> (fixed-point exec (input->device input))
    (load 0))


;part2
(let [n (let [m (-> (input->device input)
                    (store 0 1))
              m (->> m
                     (iterate exec)
                     (drop 17)
                     first)]
          (load m 4))]
  ; reverse engineered ver.
  #_(reduce + (for [i (range 1 (inc n))
                    j (range 1 (inc n))
                    :when (= (* i j) n)]
                i))

  ; which is meant to be...
  (reduce + (for [i (range 1 (+ n 1))
                  :when (zero? (mod n i))]
              i)))
