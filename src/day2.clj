(ns day2
  (:require [clojure.java.io :as io]))

(def input (->> "day2.in" io/resource io/reader line-seq))

(def input1 ["abcdef"
             "bababc"
             "abbcde"
             "abcccd"
             "aabcdd"
             "abcdee"
             "ababab"])

(def input2 ["abcde"
             "fghij"
             "klmno"
             "pqrst"
             "fguij"
             "axcye"
             "wvxyz"])


(defn fingerprint [id]
  (let [occurs (into #{} (vals (frequencies id)))]
    [(if (occurs 2) 1 0)
     (if (occurs 3) 1 0)]))

(defn part1 [input]
  (reduce * (reduce #(map + %1 %2) (map fingerprint input))))


(defn diff [x y]
  (->> (map #(- (int %1) (int %2)) x y)
       (remove zero?)
       count))

(defn common-letters [x y]
  (apply str (map #(if (= %1 %2) %1 nil) x y)))

(defn part2 [input]
  (let [[x y] (first
                (for [i input
                      j input
                      :when (= (diff i j) 1)]
                  [i j]))]
    (common-letters x y)))
