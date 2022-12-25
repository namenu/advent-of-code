(ns aoc.grid
  (:require [clojure.string :as str]))

(defn read-grid [lines]
  (->> (str/split-lines lines)
       (mapcat (fn [y line]
                 (map-indexed (fn [x v]
                                [[x y] v]) line))
               (range))))

(defn parse-grid
  ([lines] (parse-grid lines identity))
  ([lines parse-fn]
   (->> (read-grid lines)
        (map #(update % 1 parse-fn))
        (into {}))))

(defn fmap [v f]
  (mapv f v))

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

(defn manhattan-dist [a b]
  (apply + (map #(Math/abs ^Integer (- %1 %2)) a b)))

(defn bounding-box [points]
  (let [cols    (apply map vector points)
        min-max (map #(apply (juxt min max) %) cols)]
    (apply mapv vector min-max)))

(defn inside? [[lower upper] coord]
  (and (every? true? (map <= lower coord))
       (every? true? (map >= upper coord))))

(defn print-grid
  ([grid] (print-grid grid identity))
  ([grid  decoder]
   (let [[[min-x min-y] [max-x max-y]] (bounding-box (keys grid))]
     (doseq [y (range min-y (inc max-y))]
       (let [s (map (fn [x] (decoder (grid [x y]))) (range min-x (inc max-x)))]
         (println (apply str s)))))))
