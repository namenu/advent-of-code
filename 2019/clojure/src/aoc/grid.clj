(ns aoc.grid
  (:require [clojure.string :as str]
            [aoc.util :refer [bounding-box]]))

(defn parse-grid [lines]
  (->> (str/split-lines lines)
       (mapcat (fn [y line]
                 (map-indexed (fn [x v] [[x y] v]) line))
               (range))
       (into {})))

(def dirs {:north [0 1]
           :east  [1 0]
           :south [0 -1]
           :west  [-1 0]})

(defn move [pos d]
  (mapv + pos (dirs d)))

(defn adjacent-4 [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn diagonal-4 [[x y]]
  [[(dec x) (dec y)] [(inc x) (dec y)] [(dec x) (inc y)] [(inc x) (inc y)]])

(defn adjacent-8 [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]
   [(inc x) (inc y)] [(inc x) (dec y)] [(dec x) (inc y)] [(dec x) (dec y)]])

(defn print-grid [grid decoder]
  (let [[[min-x min-y] [max-x max-y]] (bounding-box (keys grid))]
    (doseq [y (range min-y (inc max-y))]
      (let [s (map (fn [x] (decoder (grid [x y]))) (range min-x (inc max-x)))]
        (println (apply str s))))))
