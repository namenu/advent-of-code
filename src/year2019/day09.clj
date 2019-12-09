;; --- Day 9: Sensor Boost ---
(ns year2019.day09
  (:require [util :refer [input]]
            [year2019.intcode :refer :all]))

; pt.1
(-> (input->state (input 2019 9))
    (add-input 1)
    (run*))

; pt.2
(-> (input->state (input 2019 9))
    (add-input 2)
    (run*))
