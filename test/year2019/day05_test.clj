(ns year2019.day05-test
  (:require [clojure.test :refer :all]
            [year2019.day05 :refer [diagnostic-code]]))

;; these tests must be generative!
(deftest part2
  (let [program [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]]
    (is (= 1 (diagnostic-code program 8)))
    (is (= 0 (diagnostic-code program 9))))
  (let [program [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]]
    (is (= 1 (diagnostic-code program 7)))
    (is (= 0 (diagnostic-code program 8))))

  (let [program [3, 3, 1108, -1, 8, 3, 4, 3, 99]]
    (is (= 1 (diagnostic-code program 8)))
    (is (= 0 (diagnostic-code program 9))))
  (let [program [3, 3, 1107, -1, 8, 3, 4, 3, 99]]
    (is (= 1 (diagnostic-code program 7)))
    (is (= 0 (diagnostic-code program 8))))

  (let [program [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]]
    (is (= 0 (diagnostic-code program 0)))
    (is (= 1 (diagnostic-code program 1))))
  (let [program [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]]
    (is (= 0 (diagnostic-code program 0)))
    (is (= 1 (diagnostic-code program 1))))

  (let [program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99]]
    (is (= 999 (diagnostic-code program 7)))
    (is (= 1000 (diagnostic-code program 8)))
    (is (= 1001 (diagnostic-code program 9))))
  )
