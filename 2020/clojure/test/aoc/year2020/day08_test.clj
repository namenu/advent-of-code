(ns aoc.year2020.day08-test
  (:require [clojure.test :refer :all]
            [aoc.year2020.day08 :refer [part1 part2]]
            [aoc.util :as aoc]))

;; these tests must be generative!
(deftest part2-test
  (let [input (aoc/input 2020 8)]
    (is (= [:infinity-loop 1928] (part1 input)))
    (is (= [:terminate 1319] (part2 input)))))
