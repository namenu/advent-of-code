(ns day2-test
  (:require [day2 :refer [part1 part2]]
            [clojure.test :refer [deftest testing is]]))

(deftest test-day2
  (testing "part1"
    (let [input ["abcdef"
                 "bababc"
                 "abbcde"
                 "abcccd"
                 "aabcdd"
                 "abcdee"
                 "ababab"]]
      (is (= 12 (part1 input)))))

  (testing "part2"
    (let [input ["abcde"
                 "fghij"
                 "klmno"
                 "pqrst"
                 "fguij"
                 "axcye"
                 "wvxyz"]]
      (is (= "fgij" (part2 input))))))
