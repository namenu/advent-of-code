(ns day5
  (:require [clojure.string :as str]))

(def lower "abcdefghijklmnopqrstuvwxyz")
(def upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def react-pattern (re-pattern (str/join "|" (map str (str lower upper) (str upper lower)))))

(defn collapse [polymer pattern]
  (str/replace polymer pattern ""))

; TODO: O(n) using stack
(defn collapsed-length [polymer]
  (->> (iterate #(collapse % react-pattern) polymer)
       (partition 2 1)
       (take-while (fn [[before after]] (not= (count before) (count after))))
       last
       second
       count))

(defn part1 [input]
  (collapsed-length input))

(defn part2 [input]
  (let [removing (map (comp re-pattern str) lower (repeat "|") upper)]
    (->> (map (partial collapse input) removing)
         (map collapsed-length)
         (apply min))))


;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test-day5
  (let [input "dabAcCaCBAcCcaDA"]
    (is (= 10 (part1 input)))
    (is (= 4 (part2 input)))))

(run-tests)
