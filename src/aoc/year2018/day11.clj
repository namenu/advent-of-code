(ns aoc.year2018.day11
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

(defn make-sum-grid [grid-serial]
  (let [level-fn (power-level grid-serial)
        sum      (fn [mem-sum x y]
                   (let [sum (fn [x y] (mem-sum mem-sum x y))]
                     (if (or (< x 1) (< y 1))
                       0
                       (let [x'   (sum (dec x) y)
                             y'   (sum x (dec y))
                             x'y' (sum (dec x) (dec y))
                             xy   (level-fn x y)]
                         (- (+ x' y' xy) x'y')))))
        mem-sum  (memoize sum)]
    (partial mem-sum mem-sum)))

(defn max-square [sum-grid n]
  (let [[[right bottom] sum]
        (apply max-key second
               (for [x (range n 301)
                     y (range n 301)]
                 (let [x'   (sum-grid (- x n) y)
                       y'   (sum-grid x (- y n))
                       x'y' (sum-grid (- x n) (- y n))
                       xy   (sum-grid x y)]
                   [[x y] (+ (- xy x' y') x'y')])))]
    [sum [(- right (dec n)) (- bottom (dec n))]]))


(defn part1 [grid-serial]
  (let [[_ [x y]] (max-square (make-sum-grid grid-serial) 3)]
    (str/join "," [x y])))

(defn part2 [grid-serial]
  (let [sum-grid (make-sum-grid grid-serial)
        max-by-n (pmap #(vector (max-square sum-grid %) %) (range 1 301))
        [[_ [x y]] n] (apply max-key ffirst max-by-n)]
    (str/join "," [x y n])))


;; tests
(require '[clojure.test :refer [deftest testing is run-tests]])

(deftest test-day11
  (testing "sample input"
    (is (= "33,45" (part1 18)))
    (is (= "21,61" (part1 42)))

    #_(is (= "90,269,16" (part2 18)))
    #_(is (= "232,251,12" (part2 42)))
    )

  (testing "memoized+pmap"
    (is (= "235,38") (time (part1 9306)))
    (is (= "233,146,13") (time (part2 9306)))
    "Elapsed time: 217.593997 msecs"
    "Elapsed time: 6861.388118 msecs"))

(run-tests)
