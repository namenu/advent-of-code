(ns aoc.cube
  (:require [aoc.grid :as grid]))

(defn adjacent?
  [[x1 y1 z1] [x2 y2 z2]]
  (= 1 (+ (abs (- x1 x2))
          (abs (- y1 y2))
          (abs (- z1 z2)))))

(defn axial-neighbors [[x y z]]
  [[(dec x) y z] [(inc x) y z]
   [x (dec y) z] [x (inc y) z]
   [x y (dec z)] [x y (inc z)]])


;; bbox

(def bounding-box grid/bounding-box)

(defn inside? [[lower upper] coord]
  (and (every? true? (map <= lower coord))
       (every? true? (map >= upper coord))))

(defn expand-1 [[lower upper]]
  [(mapv dec lower) (mapv inc upper)])
