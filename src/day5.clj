(ns day5
  (:require [clojure.string :as str]))

(def lower "abcdefghijklmnopqrstuvwxyz")
(def upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def react-pattern (re-pattern (str/join "|" (map str (str lower upper) (str upper lower)))))

(defn collapse [polymer pattern]
  (str/replace polymer pattern ""))

(defn collapse-length [polymer]
  (loop [polymer polymer]
    (let [destroyed (str/replace polymer react-pattern "")]
      (if (= (count polymer) (count destroyed))
        (count polymer)
        (recur destroyed)))))

(defn part1 [input]
  (collapse-length input))

(defn part2 [input]
  (let [removing (map (comp re-pattern str) lower (repeat "|") upper)
        removed  (map (partial collapse input) removing)]
    (apply min (map collapse-length removed))))


;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test-day5
  (let [input "dabAcCaCBAcCcaDA"]
    (is (= 10 (part1 input)))
    (is (= 4 (part2 input)))))

(run-tests)
