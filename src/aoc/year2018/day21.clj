(ns aoc.year2018.day21
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [aoc.year2018.device :refer :all]))


(def input (->> (-> "year2018/day21.in" io/resource slurp)))

(defn part1 []
  (loop [m (input->device input)]
    (if (= (:ip m) 28)
      (:registers m)
      (recur (exec m)))))

(defn part2 []
  (loop [m    (input->device input)
         seen #{}]
    (if (= (:ip m) 28)
      (let [v (load m 3)]
        #_(prn (count seen) v)
        (if (seen v)
          (:registers m)
          (recur (exec m) (conj seen v))))
      (recur (exec m) seen))))
