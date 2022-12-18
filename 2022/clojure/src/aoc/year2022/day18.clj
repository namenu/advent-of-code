(ns aoc.year2022.day18
  (:require [aoc.cube :as cube]
            [aoc.util :refer [ordered-pairs]]
            [aoc.graph :as graph]
            [clojure.string :as str]))

(defn parse-input [input]
  (let [parse-xyz (fn [s]
                    (->> (str/split s #",")
                         (mapv parse-long)))]
    (->> (str/split-lines input)
         (map parse-xyz))))

(def input "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5")

@(def input (slurp (clojure.java.io/resource "day18.in")))

(comment

  ;; pt. 1

  (let [coords (parse-input input)
        adjs   (->> (ordered-pairs coords)
                    (filter #(apply cube/adjacent? %))
                    )]
    (- (* (count coords) 6)
       (* (count adjs) 2))
    )
  ;; => 64


  ;; pt.2
  (let [coords (parse-input input)
        lava   (set coords)
        bbox   (cube/expand-1 (cube/bounding-box lava))
        steam  (keys (graph/bfs (first bbox)
                                (fn [cur]
                                  (->> (cube/axial-neighbors cur)
                                       (filter #(cube/inside? bbox %))
                                       (remove lava)))))]
    (->> (for [a steam
               b lava
               :when (cube/adjacent? a b)]
           [a b])
         count))
  ;; => 58

  )
