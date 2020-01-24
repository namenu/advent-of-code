(ns aoc.year2019.day02-test
  (:require [clojure.test :refer :all]
            [aoc.year2019.day02 :refer [part1 part2]]))

(deftest part1-test
  (are [expected actual] (= expected actual)
    3500 (part1 [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50])
    2 (part1 [1, 0, 0, 0, 99])
    30 (part1 [1, 1, 1, 4, 99, 5, 6, 0, 99])
    ))
