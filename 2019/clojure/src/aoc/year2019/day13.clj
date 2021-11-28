;; --- Day 13: Care Package ---
(ns aoc.year2019.day13
  (:require [aoc.util :refer [input countp]]
            [aoc.grid :refer [parse-grid]]
            [aoc.year2019.intcode :refer :all]
            [quil.core :as q]
            [quil.middleware :refer [fun-mode]]))

; pt.1
(comment
  (let [data (-> (input->machine (input 2019 13))
                 (run)
                 (:output))
        grid (->> (partition 3 data)
                  (reduce (fn [grid [x y v]]
                            (assoc grid [x y] v))
                          {}))]
    (aoc.grid/print-grid grid identity)

    (countp #(= 2 (second %)) grid)))

; pt.2
(defn init-state []
  {:game (-> (parse-program (input 2019 13))
             (assoc 0 2)
             (->machine))
   :data {:grid {}}})

(defn input-move [data]
  (let [[bx _] (:ball data)
        [px _] (:paddle data)]
    (compare bx px)))

(defn next-frame [{:keys [game data] :as state}]
  (let [[game' output] (-> game (run) (get-output))
        data (reduce (fn [data [x y v]]
                       (if (= x -1)
                         (assoc data :score v)
                         (cond-> (update data :grid assoc [x y] v)
                           (= v 3) (assoc :paddle [x y])
                           (= v 4) (assoc :ball [x y]))))
                     data
                     (partition 3 output))]
    (assoc state :game (add-input game' (input-move data))
                 :data data)))

(let [final-state (->> (iterate next-frame (init-state))
                       (next)
                       (drop-while (fn [{:keys [data]}]
                                     (let [nbricks (aoc.util/countp #(= 2 (second %)) (:grid data))]
                                       (pos? nbricks))))
                       (first))]
  (get-in final-state [:data :score]))


(defn draw [{:keys [data]}]
  (q/background 0)
  (q/no-stroke)
  (let [grid        (:grid data)
        scale       11]
    (doseq [[pos v] grid
            :let [[x y] (map #(* scale %) pos)]]
      (case v
        0 (q/fill 0 0 0)
        1 (q/fill 100 100 100)
        2 (q/fill 255 255 0)
        3 (q/fill 0 0 255)
        4 (q/fill 255 0 0))
      (q/rect x y scale scale))))

(comment
  (defn ^:export run-sketch []
    (q/defsketch day13
      :size [500 250]
      :setup init-state
      :update next-frame
      :draw draw
      :features [:keep-on-top]
      :middleware [fun-mode]))

  (run-sketch))
