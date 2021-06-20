;; --- Day 3: Crossed Wires ---
(ns aoc.year2019.day03
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [aoc.util :refer [input-lines manhattan-dist]]))

(defn parse-wire [wire]
  (let [dir-map       {"R" :right, "U" :up, "L" :left, "D" :down}
        parse-segment #(let [[_ dir scalar] (re-find #"([RULD])(\d+)" %)]
                         [(dir-map dir) (Integer/parseInt scalar)])]
    (->> (str/split wire #",")
         (map parse-segment))))

(defn move [[x y] dir]
  (case dir
    :up [x (inc y)]
    :down [x (dec y)]
    :left [(dec x) y]
    :right [(inc x) y]))

(defn move-segment [pos [dir scalar]]
  (drop 1 (reductions move pos (repeat scalar dir))))

(defn wire->trail [start-pos wire]
  (let [segments (parse-wire wire)]
    (reduce (fn [coords segment]
              (let [tip   (peek coords)
                    trail (move-segment tip segment)]
                (into coords trail)))
            [start-pos]
            segments)))

(def start-pos [0 0])

(comment
  (move-segment start-pos [:up 3])

  (let [[wire1 wire2] (input-lines 2019 3)
        [t1 t2] [(wire->trail start-pos wire1) (wire->trail start-pos wire2)]
        crosses (disj (set/intersection (set t1) (set t2)) start-pos)]
    ; pt.1
    (apply min (map #(apply manhattan-dist [start-pos %]) crosses))

    ; pt.2
    (let [[d1 d2] (map #(into {} (map vector % (range))) [t1 t2])]
      (apply min (map #(+ (d1 %) (d2 %)) crosses))))
  )
