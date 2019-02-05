;; --- Day 5: A Maze of Twisty Trampolines, All Alike ---
(ns year2017.day05
  (:require [util :refer [find-first]]
            [clojure.java.io :as io]))

(def state {:jumps [0
                    3
                    0
                    1
                    -3]
            :pos   0})

(defn input->state []
  (let [jumps (->> (-> "year2017/day05.in" io/resource io/reader line-seq)
                   (mapv #(Integer/parseInt %)))]
    {:jumps jumps
     :pos   0}))

(defn step [{:keys [jumps pos] :as state}]
  (let [jump   (nth jumps pos)
        offset (if (>= jump 3) -1 +1)]
    (-> state
        (update :pos + jump)
        (update-in [:jumps pos] + offset))))

(defn out-of-range [{:keys [jumps pos]}]
  (not (< -1 pos (count jumps))))

(->> state
     #_(input->state)
     (iterate step)
     (map-indexed vector)
     (find-first (comp out-of-range second))
     (first))