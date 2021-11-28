(ns aoc.year2018.day04-test
  (:require [clojure.test :refer [deftest is]]
            [aoc.year2018.day04 :refer [part1 part2]]
            [aoc.util :refer [input-lines]]))

(deftest test-day4-sample
  (let [input ["[1518-11-01 00:00] Guard #10 begins shift"
               "[1518-11-01 00:05] falls asleep"
               "[1518-11-01 00:25] wakes up"
               "[1518-11-01 00:30] falls asleep"
               "[1518-11-01 00:55] wakes up"
               "[1518-11-01 23:58] Guard #99 begins shift"
               "[1518-11-02 00:40] falls asleep"
               "[1518-11-02 00:50] wakes up"
               "[1518-11-03 00:05] Guard #10 begins shift"
               "[1518-11-03 00:24] falls asleep"
               "[1518-11-03 00:29] wakes up"
               "[1518-11-04 00:02] Guard #99 begins shift"
               "[1518-11-04 00:36] falls asleep"
               "[1518-11-04 00:46] wakes up"
               "[1518-11-05 00:03] Guard #99 begins shift"
               "[1518-11-05 00:45] falls asleep"
               "[1518-11-05 00:55] wakes up"]]
    (is (= 240 (part1 input)))
    (is (= 4455 (part2 input)))))

(deftest test-day4
  (let [input (input-lines 2018 4)]
    (is (= 63509 (part1 input)))
    (is (= 47910 (part2 input)))))


(clojure.test/run-tests)