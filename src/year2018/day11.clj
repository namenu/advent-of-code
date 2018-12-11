(ns year2018.day11
  (:require [clojure.string :as str]))

(defn power-level [grid-serial]
  (fn [x y]
    (let [rack-id (+ x 10)]
      (-> rack-id
          (* y)
          (+ grid-serial)
          (* rack-id)
          (quot 100)
          (mod 10)
          (- 5)))))

(def f (power-level 9306))

; TODO: memoize by power-grid.
(def sum-1x1
  (memoize
    (fn [x y]
      (if (or (< x 1) (< y 1))
        0
        (let [x'   (sum-1x1 (dec x) y)
              y'   (sum-1x1 x (dec y))
              x'y' (sum-1x1 (dec x) (dec y))
              xy   (f x y)]
          (- (+ x' y' xy) x'y'))))))

(defn max-square [n]
  (let [[[right bottom] sum]
        (apply max-key second
               (for [x (range n 301)
                     y (range n 301)]
                 (let [x'   (sum-1x1 (- x n) y)
                       y'   (sum-1x1 x (- y n))
                       x'y' (sum-1x1 (- x n) (- y n))
                       xy   (sum-1x1 x y)]
                   [[x y] (+ (- xy x' y') x'y')])))]
    [sum [(- right (dec n)) (- bottom (dec n))]]))


(defn part1 [grid-serial]
  (let [[_ [x y]] (max-square 3)]
    (str/join "," [x y])))

(defn part2 [grid-serial]
  (let [[[_ [x y]] n]
        (apply max-key ffirst (pmap #(vector (max-square %) %) (range 1 301)))]
    (str/join "," [x y n])))


;; tests
(require '[clojure.test :refer [deftest testing is run-tests]])

(deftest test-day11
  (testing "memoized+pmap"
    (is (= "235,38") (time (part1 9306)))
    (is (= "233,146,13") (time (part2 9306)))
    "Elapsed time: 217.593997 msecs"
    "Elapsed time: 6861.388118 msecs"))

(run-tests)
