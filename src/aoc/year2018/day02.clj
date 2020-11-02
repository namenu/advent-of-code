(ns aoc.year2018.day02
  (:require [aoc.util :refer [find-first countp ordered-pairs]]))

(defn fingerprint [id]
  (let [occurs (into #{} (vals (frequencies id)))]
    [(if (occurs 2) 1 0)
     (if (occurs 3) 1 0)]))

(defn part1 [input]
  (reduce * (reduce #(map + %1 %2) (map fingerprint input))))

(defn num-diffs [x y]
  (->> (map = x y)
       (countp false?)))

(defn common-letters [x y]
  (apply str (map #(if (= %1 %2) %1 nil) x y)))

(defn part2 [input]
  (let [[x y] (->> (ordered-pairs input)
                   (find-first (fn [[x y]]
                                 (= (num-diffs x y) 1))))]
    (common-letters x y)))

;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test-day2
  (let [input ["abcdef"
               "bababc"
               "abbcde"
               "abcccd"
               "aabcdd"
               "abcdee"
               "ababab"]]
    (is (= 12 (part1 input))))

  (let [input ["abcde"
               "fghij"
               "klmno"
               "pqrst"
               "fguij"
               "axcye"
               "wvxyz"]]
    (is (= "fgij" (part2 input)))))

(run-tests)
