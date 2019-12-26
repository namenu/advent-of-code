;; --- Day 11: Space Police ---
(ns aoc.year2019.day11
  (:require [aoc.util :refer [input cart->polar]]
            [aoc.year2019.intcode :refer :all]))

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
  (or (get panels pos) 0))

(def panels (atom {}))
(def robot (atom {:pos    [0 0]
                  :dir    [0 1]
                  :buffer nil}))

(defn reset-state []
  (reset! panels {})
  (reset! robot {:pos    [0 0]
                 :dir    [0 1]
                 :buffer nil}))

(defn input-fn []
  ;(prn "input cb")
  (read-panel @panels (:pos @robot)))

(defn output-fn [output]
  ;(prn "output cb" output)
  (if-let [color (:buffer @robot)]
    (do
      (swap! panels assoc (:pos @robot) color)
      (swap! robot turn output)
      (swap! robot move)
      (swap! robot assoc :buffer nil))
    (swap! robot assoc :buffer output)))


; pt.1
(let [in    (input 2019 11)
      state (-> (input->state in)
                (assoc :input-fn input-fn)
                (assoc :output-fn output-fn))
      ]
  (reset-state)
  (swap! panels assoc [0 0] 1)
  (run* state)
  (prn (count @panels))

  (doseq [y (range 5 -10 -1)]
    (let [s (map (fn [x]
                   (if (zero? (read-panel @panels [x y]))
                     "◾️"
                     "▫️️️️"))
                 (range -5 50))]
      (println (apply str s)))))
