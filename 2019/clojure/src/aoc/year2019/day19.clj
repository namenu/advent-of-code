;; --- Day 19: Tractor Beam ---
(ns aoc.year2019.day19
  (:require [aoc.util :refer [input find-first]]
            [aoc.grid :refer [parse-grid print-grid]]
            [aoc.year2019.intcode :refer :all]))


(defn run-output1 [state0]
  (let [o-cnt (count (:output state0))]
    (->> (iterate run state0)
         (find-first #(not= (count (:output %)) o-cnt))
         (:output)
         (peek))))

(def grid
  (let [drone (input->machine (input 2019 19))]
    (->> (for [x (range 50)
               y (range 50)
               :let [output (-> drone (add-input x) (add-input y) (run-output1))]]
           [[x y] output])
         (into {}))))

; pt.1
(count (filter #(= 1 (second %)) grid))

(print-grid grid identity)

; pt.2
(let [drone (input->machine (input 2019 19))
      beam? (fn [x y]
              (= 1 (-> drone (add-input x) (add-input y) (run-output1))))
      n     100]
  (loop [x 44
         y 50]
    (cond
      ; try move v & nudge >
      (not (beam? (+ x n -1) y))
      (let [[x y] (loop [x x y (inc y)]
                    (if (not (beam? x y))
                      (recur (inc x) y)
                      [x y]))]
        (recur x y))

      ; try move > & nudge v
      (not (beam? x (+ y n -1)))
      (let [[x y] (loop [x (inc x) y y]
                    (if (not (beam? x y))
                      (recur x (inc y))
                      [x y]))]
        (recur x y))

      :else
      [x y])))
