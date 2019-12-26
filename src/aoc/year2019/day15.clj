;; --- Day 15: Oxygen System ---
(ns aoc.year2019.day15
  (:require [aoc.util :refer [input find-first bounding-box range-incl]]
            [aoc.graph :refer [bfs]]
            [aoc.grid :refer [print-grid]]
            [aoc.year2019.intcode :refer :all]))

(def dirs {1 [0 1]
           2 [0 -1]
           3 [-1 0]
           4 [1 0]})

(def opposite-dir {1 2, 2 1, 3 4, 4 3})

(defn run-output [state0]
  (let [o-cnt (count (:output state0))]
    (->> (iterate run state0)
         (find-first #(not= (count (:output %)) o-cnt)))))

(defn move [pos d]
  (mapv + pos (dirs d)))

(defn move-droid [droid d]
  (let [droid' (-> droid (add-input d) (run-output))]
    [droid' (peek (:output droid'))]))

(defn probe [droid grid pos dist]
  (loop [[d & ds] [1 2 3 4]
         droid droid
         grid  grid]
    (if d
      (let [pos' (move pos d)]
        (if (grid pos')
          (recur ds droid grid)

          (let [[droid' output] (move-droid droid d)
                grid' (assoc grid pos' output)]

            (if (zero? output)
              (recur ds droid grid')
              (let [grid'' (probe droid' grid' pos' (inc dist))
                    [droid'' _] (move-droid droid' (opposite-dir d))]
                (recur ds droid'' grid''))))))
      grid)))

(let [droid       (input->state (input 2019 15))
      init-grid   {[0 0] 1}
      start-pos   [0 0]
      grid        (probe droid init-grid start-pos 0)
      adjacent-fn (fn [pos]
                    (->> (map #(move pos %) [1 2 3 4])
                         (remove #(zero? (grid %)))))]

  (let [decoder #(get {1 "▫️️️️" 2 "⭐️"} % "◾️")]
    (print-grid grid decoder))

  (let [station-pos [18 18]]
    ; pt.1
    ((bfs start-pos adjacent-fn) station-pos)

    ; pt.2
    (apply max (vals (bfs start-pos adjacent-fn)))))
