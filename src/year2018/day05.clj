(ns year2018.day05
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

(def lower "abcdefghijklmnopqrstuvwxyz")
(def upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def react-pattern (re-pattern (str/join "|" (map str (str lower upper) (str upper lower)))))

(defn collapse [polymer pattern]
  (str/replace polymer pattern ""))

; O(n^2)
(defn collapse-all [polymer]
  (->> (iterate #(collapse % react-pattern) polymer)
       (partition 2 1)
       (take-while (fn [[before after]] (not= (count before) (count after))))
       last
       second))

(defn reacts? [x y]
  (and (not= x y)
       (= (str/lower-case x) (str/lower-case y))))

(defn add-unit [polymer unit]
  (if (some-> (peek polymer) (reacts? unit))
    (pop polymer)
    (conj polymer unit)))

(defn merge-polymers
  ([] [])
  ([left right]
   (loop [left  left
          right right]
     (let [unit (first right)]
       (if (and unit (some-> (peek left) (reacts? unit)))
         (recur (pop left) (next right))
         (into left right))))))

; O(nlogn)
(defn part1 [input]
  (count (r/fold merge-polymers add-unit (vec input))))

(defn part2 [input]
  (let [removing (map (comp re-pattern str) lower (repeat "|") upper)]
    (->> (map (partial collapse input) removing)
         (pmap part1)
         (apply min))))

;; tests
(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.java.io :as io])

(deftest test-day5
  (let [input "dabAcCaCBAcCcaDA"]
    (is (= 10 (part1 input)))
    (is (= 4 (part2 input))))

  (let [input (->> "day05.in" io/resource io/reader slurp)]
    (is (= 11310 (part1 input)))
    (is (= 6020 (part2 input)))))

(run-tests)
