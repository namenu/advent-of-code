;; --- Day 3: Spiral Memory ---
(ns year2017.day03
  (:require [util :refer [find-first manhattan-dist]]))


;; scales = 1 1 2 2 3 3 4 4
(def scales
  (let [nat (drop 1 (range))]
    (interleave nat nat)))

;; dirs = R U L D R U L D ...
(def dirs
  (cycle [[1 0]
          [0 -1]
          [-1 0]
          [0 1]]))

(defn spiral []
  (let [moves (mapcat repeat scales dirs)]
    (reductions #(mapv + %1 %2) [0 0] moves)))

;; pt.1
(defn move [pos scale dir]
  (mapv + pos (mapv (partial * scale) dir)))

(defn spiral-seq
  "[(0 0) 1] [(1 0) 2] [(1 -1) 3]"
  []
  (let [step (fn step [[pos acc] [s & ss] [d & ds]]
               (lazy-seq
                 (cons [pos acc d]
                       (let [pos' (map + pos (map (partial * s) d))]
                         (step [pos' (+ acc s)] ss ds)))))]
    (step [[0 0] 1] scales dirs)))

(defn spiral-dist [n]
  (let [[pos moved dir] (->> (spiral-seq)
                             (take-while (fn [[_ acc]]
                                           (< acc n)))
                             (last))]
    (manhattan-dist [0 0] (move pos (- n moved) dir)))

  ;; OR, just
  #_(manhattan-dist [0 0] (nth (spiral) (dec n))))

(def n 312051)
(spiral-dist n)

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