;; --- Day 15: Oxygen System ---
(ns year2019.day15
  (:require [util :refer [input find-first bounding-box range-incl]]
            [graph :refer [bfs]]
            [year2019.intcode :refer :all]))

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

(defn render-grid [grid]
  (let [[[min-x min-y] [max-x max-y]] (bounding-box (keys grid))]
    (doseq [y (range-incl min-y max-y)]
      (let [s (map (fn [x]
                     (case (get grid [x y] 0)
                       0 "◾️"
                       1 "▫️️️️"
                       2 "⭐️"))
                   (range-incl min-x max-x))]
        (println (apply str s))))))

(let [droid       (input->state (input 2019 15))
      init-grid   {[0 0] 1}
      start-pos   [0 0]
      grid        (probe droid init-grid start-pos 0)
      adjacent-fn (fn [pos]
                    (->> (map #(move pos %) [1 2 3 4])
                         (remove #(zero? (grid %)))))]

  (render-grid grid)

  (let [station-pos [18 18]]
    ; pt.1
    ((bfs start-pos adjacent-fn) station-pos)

    ; pt.2
    (apply max (vals (bfs start-pos adjacent-fn)))))
