;; --- Day 5: Sunny with a Chance of Asteroids ---
(ns aoc.year2019.day05
  (:require [aoc.util :refer [input-nums]]
            [aoc.year2019.intcode :refer :all]))

(def diagnostic-code (comp last run-pure-program))

(comment
  (let [program (input-nums 2019 5 ",")]
    ; pt.1
    (prn (diagnostic-code program 1))

    ; pt.2
    (prn (diagnostic-code program 5))))
