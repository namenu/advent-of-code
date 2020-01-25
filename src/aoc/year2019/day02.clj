;; --- Day 2: 1202 Program Alarm ---
(ns aoc.year2019.day02
  (:require [aoc.util :refer [input-nums find-first]]
            [aoc.year2019.intcode :refer :all]
            [clojure.core.async :as async]))

(defn part1
  ([program]
   (let [res (-> (run-program program (async/chan) (async/chan))
                 (async/<!!))]
     (get-in res [:program 0])))
  ([program noun verb]
   (part1 (assoc program 1 noun 2 verb))))

(defn part2 [program output]
  (first
    (for [n (range 100)
          v (range 100)
          :when (= (part1 program n v) output)]
      (+ (* 100 n) v))))

(comment
  (let [program (input-nums 2019 2 ",")]
    ; pt.1
    (prn (part1 program 12 2))

    ; pt.2
    (prn (part2 program 19690720))
    ))
