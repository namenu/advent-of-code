(ns aoc.year2022.day09
  (:require [clojure.string :as str]))

(def input "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(def state0 {:head  [0 0]
             :tail1  [0 0]
             :tail2  [0 0]
             :tail3  [0 0]
             :tail4  [0 0]
             :tail5  [0 0]
             :tail6  [0 0]
             :tail7  [0 0]
             :tail8  [0 0]
             :tail9  [0 0]})

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

(defn move [{:keys [head tail1 tail2 tail3 tail4 tail5 tail6 tail7 tail8 tail9] :as state} d]
  (let [head (move-dir head d)
        tail1 (move-toward tail1 head)
        tail2 (move-toward tail2 tail1)
        tail3 (move-toward tail3 tail2)
        tail4 (move-toward tail4 tail3)
        tail5 (move-toward tail5 tail4)
        tail6 (move-toward tail6 tail5)
        tail7 (move-toward tail7 tail6)
        tail8 (move-toward tail8 tail7)
        tail9 (move-toward tail9 tail8)]
    (assoc state :head head
                 :tail1 tail1
                 :tail2 tail2
                 :tail3 tail3
                 :tail4 tail4
                 :tail5 tail5
                 :tail6 tail6
                 :tail7 tail7
                 :tail8 tail8
                 :tail9 tail9)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map #(update % 1 parse-long))))

(def input (slurp (clojure.java.io/resource "day09.in")))

(def input "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20")

;; part2
(let [moveseq (->> (parse-input input)
                   (mapcat (fn [[dir times]] (repeat times dir))))]
  (->> (reductions move state0 moveseq)
       (map :tail9)
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