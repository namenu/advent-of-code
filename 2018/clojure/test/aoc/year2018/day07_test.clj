(ns aoc.year2018.day07-test
  (:require [clojure.test :refer [deftest testing is]]
            [aoc.year2018.day07 :refer [part1 part2]]
            [aoc.year2018.day07-2 :refer [part1 part2] :rename {part1 part1-2 part2 part2-2}]
            [aoc.util :refer [input-lines]]))

(deftest test-day7
  (let [sample-input ["Step C must be finished before step A can begin."
                      "Step C must be finished before step F can begin."
                      "Step A must be finished before step B can begin."
                      "Step A must be finished before step D can begin."
                      "Step B must be finished before step E can begin."
                      "Step D must be finished before step E can begin."
                      "Step F must be finished before step E can begin."]
        input        (input-lines 2018 7)]

    (testing "trial-1"
      (is (= "CABDFE" (part1 sample-input)))
      (is (= "CQSWKZFJONPBEUMXADLYIGVRHT" (part1 (input-lines 2018 7))))

      (is (= 15 (part2 sample-input 2 0)))
      (is (= 914 (part2 input 5 60))))

    (testing "trial-2"
      (is (= "CABDFE" (part1-2 sample-input)))
      (is (= "CQSWKZFJONPBEUMXADLYIGVRHT" (part1-2 (input-lines 2018 7))))

      (is (= 15 (part2-2 sample-input 2 0)))
      (is (= 914 (part2-2 input 5 60))))))

(clojure.test/run-tests)
