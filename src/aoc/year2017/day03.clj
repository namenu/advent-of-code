;; --- Day 3: Spiral Memory ---
(ns aoc.year2017.day03
  (:require [aoc.util :refer [find-first manhattan-dist]]))


;; scales = 1 1 2 2 3 3 4 4
(def scales
  (let [nat (drop 1 (range))]
    (interleave nat nat)))

;; dirs = R U L D R U L D ...
(def dirs
  (cycle [[1 0] [0 -1] [-1 0] [0 1]]))

(defn spiral []
  (let [moves (mapcat repeat scales dirs)]
    (reductions #(mapv + %1 %2) [0 0] moves)))

(def n 312051)

;; pt.1
(manhattan-dist [0 0] (nth (spiral) (dec n)))

;; pt.2
(defn neighbors [[x y]]
  [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
   [x (dec y)] [x (inc y)]
   [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])

(defn acc-spiral []
  (letfn [(step [acc [pos & next]]
            (lazy-seq
              (let [v (apply + (vals (select-keys acc (neighbors pos))))]
                (cons v (step (assoc acc pos v) next)))))]
    (step {[0 0] 1} (next (spiral)))))

(find-first #(> % n) (acc-spiral))
