(ns aoc.year2021.day11
  (:require [aoc.grid :as g]
            [medley.core :refer [map-vals filter-vals]]
            ))

(def input "11111\n19991\n19191\n19991\n11111")
(def size-x 5)
(def size-y 5)

(def input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")
(def input "4134384626\n7114585257\n1582536488\n4865715538\n5733423513\n8532144181\n1288614583\n2248711141\n6415871681\n7881531438")
(def size-x 10)
(def size-y 10)

(defn adjacent-8 [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]
   [(inc x) (inc y)] [(inc x) (dec y)] [(dec x) (inc y)] [(dec x) (dec y)]])

(defn step [grid]
  (let [phase-1 #(update-vals % inc)

        phase-2 (fn [grid]
                  (loop [grid    grid
                         flashed #{}]
                    (if-let [flashing (seq (keep (fn [[xy lvl]]
                                                   (when (and (> lvl 9) (not (flashed xy)))
                                                     xy)) grid))]
                      (let [also (->> flashing
                                      (mapcat adjacent-8)
                                      (filter (fn [[x y]]
                                                (and (<= 0 x (dec size-x))
                                                     (<= 0 y (dec size-y))))))]
                        (recur (reduce #(update %1 %2 inc) grid also)
                               (into flashed flashing)))
                      grid)))

        phase-3 #(update-vals % (fn [lvl] (if (> lvl 9) 0 lvl)))]
    (-> grid phase-1 phase-2 phase-3)))

(defn count-flashed [grid]
  (->> (filter (fn [[_ lvl]] (zero? lvl)) grid)
       count))

(defn synchronized? [grid]
  (= (count-flashed grid) (* size-x size-y)))

;; part1
(let [grid (g/parse-grid input (comp parse-long str))]
  (->> (iterate step grid)
       next
       (take 100)
       (map count-flashed)
       (apply +)))


;; part2
(let [grid (g/parse-grid input (comp parse-long str))]
  (.indexOf (->> grid
                 (iterate step)
                 (map synchronized?))
            true))