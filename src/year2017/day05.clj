;; --- Day 5: A Maze of Twisty Trampolines, All Alike ---
(ns year2017.day05
  (:require [util :refer [find-first]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def input "0\n3\n0\n1\n-3")
(def input (-> "year2017/day05.in" io/resource slurp))

(defn input->state [input]
  {:jumps (->> (str/split-lines input)
               (mapv #(Integer/parseInt %)))
   :pos   0})

(defn step [offset-fn {:keys [jumps pos] :as state}]
  (let [jump   (nth jumps pos)]
    (-> state
        (update :pos + jump)
        (update-in [:jumps pos] (offset-fn jump)))))

(defn out-of-range [{:keys [jumps pos]}]
  (not (< -1 pos (count jumps))))

(defn solve [input offset-fn]
  (->> (input->state input)
       (iterate (partial step offset-fn))
       (map-indexed vector)
       (find-first (comp out-of-range second))
       (first)))

; pt.1
(solve input (constantly inc))

; pt.2
(solve input (fn [v] (if (>= v 3) dec inc)))