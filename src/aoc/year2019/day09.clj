;; --- Day 9: Sensor Boost ---
(ns aoc.year2019.day09
  (:require [aoc.util :refer [input]]
            [aoc.year2019.intcode :refer :all]))

; pt.1
(-> (input->machine (input 2019 9))
    (add-input 1)
    (run))

; pt.2
(-> (input->machine (input 2019 9))
    (add-input 2)
    (run))
