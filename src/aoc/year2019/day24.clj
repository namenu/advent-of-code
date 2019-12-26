;; --- Day 24: Planet of Discord ---
(ns aoc.year2019.day24
  (:require [aoc.util :refer [input first-duplicate-index countp]]
            [aoc.grid :refer [parse-grid adjacent-4]]
            [clojure.core.match :refer [match]]))

(def bug? #(= \# %))

(defn live-or-die [cur nbugs]
  (if (bug? cur)
    (if (= 1 nbugs) \# \.)
    (if (<= 1 nbugs 2) \# \.)))

(defn next-minute [grid]
  (reduce-kv (fn [g pos v]
               (let [nbugs (countp #(bug? (grid %)) (adjacent-4 pos))]
                 (assoc g pos (live-or-die v nbugs))))
             {} grid))

(defn biodiversity [grid]
  (->> (filter #(bug? (second %)) grid)
       (map (fn [[[x y] _]]
              (let [p (+ (* y 5) x)]
                (long (Math/pow 2 p)))))
       (apply +)))

; pt.1
(let [grid      (parse-grid "....#\n#..#.\n#..##\n..#..\n#....")
      grid      (parse-grid (input 2019 24))
      iteration (iterate next-minute grid)]
  (prn (first-duplicate-index iteration))
  ; => 39
  (biodiversity (nth iteration 39)))


; pt.2
(defn next-minute-recur [grid inner-grid outer-grid]
  (let [adj-fn (fn [[px py]]
                 (->> (adjacent-4 [px py])
                      (mapcat (fn [[x y]]
                                (cond
                                  (< x 0) [(outer-grid [1 2])]
                                  (> x 4) [(outer-grid [3 2])]
                                  (< y 0) [(outer-grid [2 1])]
                                  (> y 4) [(outer-grid [2 3])]
                                  (and (= x 2) (= y 2))
                                  (cond
                                    (= py 1) (map #(inner-grid [%1 %2]) (range 5) (repeat 0))
                                    (= py 3) (map #(inner-grid [%1 %2]) (range 5) (repeat 4))
                                    (= px 1) (map #(inner-grid [%1 %2]) (repeat 0) (range 5))
                                    (= px 3) (map #(inner-grid [%1 %2]) (repeat 4) (range 5)))
                                  :else [(grid [x y])])))))]
    (reduce-kv (fn [g pos v]
                 (let [nbugs (countp bug? (adj-fn pos))]
                   (assoc g pos (live-or-die v nbugs))))
               {} grid)))

(def grid-at-0-0
  (let [grid (parse-grid "....#\n#..#.\n#.?##\n..#..\n#....\n")
        grid (parse-grid (input 2019 24))]
    (dissoc grid [2 2])))

(def empty-grid (zipmap (keys grid-at-0-0) (repeat \.)))

(def grid-at
  (memoize
    (fn [level minute]
      (match [minute level]
        [0 0] grid-at-0-0
        [0 _] empty-grid
        [_ _] (next-minute-recur
                (grid-at level (dec minute))
                (grid-at (inc level) (dec minute))
                (grid-at (dec level) (dec minute)))))))

(let [minute 200]
  (->> (map #(grid-at % minute) (range -100 101))
       (map #(countp bug? (vals %)))
       (apply +)))
