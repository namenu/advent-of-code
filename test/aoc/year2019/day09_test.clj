(ns aoc.year2019.day09-test
  (:require [aoc.util :refer [input-nums]]
            [aoc.year2019.day09 :refer [run-with-mode]]
            [clojure.test :refer :all]))

(deftest day09-test
  (let [program [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]]
    (is (= 109 (run-with-mode program 0))))
  (let [program [1102, 34915192, 34915192, 7, 4, 7, 99, 0]]
    (is (= 1219070632396864 (run-with-mode program 0))))
  (let [program [104, 1125899906842624, 99]]
    (is (= 1125899906842624 (run-with-mode program 0))))

  (testing "boost mode"
    (let [program (input-nums 2019 9 ",")]
      (is (= 4080871669 (run-with-mode program 1)))
      (is (= 75202 (run-with-mode program 2))))))
