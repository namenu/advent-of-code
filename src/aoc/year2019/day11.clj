;; --- Day 11: Space Police ---
(ns aoc.year2019.day11
  (:require [aoc.util :refer [input-nums cart->polar]]
            [aoc.year2019.intcode :refer :all]
            [clojure.core.async :as async]))

(defn move [{:keys [pos dir] :as robot}]
  (let [[x y] pos
        [dx dy] dir]
    (assoc robot :pos [(+ x dx) (+ y dy)])))

(defn turn [{:keys [dir] :as robot} lr]
  (let [[dx dy] dir]
    (if (zero? lr)
      (assoc robot :dir [(- dy) dx])
      (assoc robot :dir [dy (- dx)]))))

(defn read-panel [panels pos]
  (get panels pos 0))

(defn update-state [{:keys [robot] :as state} color direction]
  (-> state
      (update :panels assoc (:pos robot) color)
      (update :robot turn direction)
      (update :robot move)))

(defn probe [start-color]
  (let [in  (async/chan 1)
        out (async/chan)]
    (run-program! (input-nums 2019 11 ",") {:in in :out out})
    (loop [state {:panels {[0 0] start-color}
                  :robot  {:pos [0 0]
                           :dir [0 1]}}]
      (let [input (read-panel (:panels state) (get-in state [:robot :pos]))]
        (async/>!! in input))

      (let [o1 (async/<!! out)
            o2 (async/<!! out)]
        (if o1
          (recur (update-state state o1 o2))
          state)))))

(comment
  ; pt.1
  (let [state  (probe 0)
        panels (:panels state)]
    (prn (count panels)))

  ; pt.2
  (let [state  (probe 1)
        panels (:panels state)]
    (doseq [y (range 5 -10 -1)]
      (let [s (map (fn [x]
                     (if (zero? (read-panel panels [x y]))
                       "◾️"
                       "▫️️️️"))
                   (range -5 50))]
        (println (apply str s))))))
