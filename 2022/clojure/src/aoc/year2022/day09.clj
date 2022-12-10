(ns aoc.year2022.day09
  (:require [clojure.string :as str]))

(def input "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(def state0 {:head  [0 0]
             :tail  [0 0]})

(defn move-dir [pos dir]
  (mapv + pos (case dir
                "R" [1 0]
                "U" [0 1]
                "L" [-1 0]
                "D" [0 -1])))

(defn move-toward [tail head]
  (let [[dx dy] (mapv - head tail)]
    (if (= (+ (abs dx) (abs dy)) 3)
      ;; diagonal
      (cond-> tail
              (> dx 0) (move-dir "R")
              (< dx 0) (move-dir "L")
              (> dy 0) (move-dir "U")
              (< dy 0) (move-dir "D"))
      ;; axial
      (cond-> tail
              (> dx 1) (move-dir "R")
              (< dx -1) (move-dir "L")
              (> dy 1) (move-dir "U")
              (< dy -1) (move-dir "D")))))

(defn move [{:keys [head tail] :as state} d]
  (let [head (move-dir head d)
        tail (move-toward tail head)]
    (assoc state :head head :tail tail)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map #(update % 1 parse-long))))

(def input (slurp (clojure.java.io/resource "day09.in")))
(let [moveseq (->> (parse-input input)
                   (mapcat (fn [[dir times]] (repeat times dir))))]
  (->> (reductions move state0 moveseq)
       (map :tail)
       set
       count))

(comment
  (repeat 4 "X")

  (-> state
      (move "R")
      (move "R")
      (move "R")
      (move "R")
      (move "U")
      (move "U")
      (move "U")
      (move "U"))

  (-> {:head [4 1] :tail [3 0]}
      (move "U")))