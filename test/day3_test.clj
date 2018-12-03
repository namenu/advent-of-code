(ns day3-test
  (:require [day3 :refer [part1 part2]]
            [clojure.test :refer [deftest testing is]]))

(def input ["#1 @ 1,3: 4x4"
            "#2 @ 3,1: 4x4"
            "#3 @ 5,5: 2x2"])

(deftest test-day3
  (is (= 4 (part1 input)))
  (is (= 3 (part2 input))))
