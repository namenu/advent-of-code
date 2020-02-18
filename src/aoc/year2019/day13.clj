;; --- Day 13: Care Package ---
(ns aoc.year2019.day13
  (:require [aoc.util :refer [input-nums countp]]
            [aoc.grid :refer [parse-grid print-grid]]
            [aoc.year2019.intcode :refer :all]
            [clojure.core.async :as async]
            [quil.core :as q]
            [quil.middleware :refer [fun-mode]]))

; pt.1
(comment
  (let [data (run-pure-program (input-nums 2019 13 ","))
        grid (->> (partition 3 data)
                  (reduce (fn [grid [x y v]]
                            (assoc grid [x y] v))
                          {}))]
    (print-grid grid identity)

    (countp #(= 2 (second %)) grid)))

; pt.2
(defn init-state []
  (let [in      (async/chan)
        out     (async/chan 100 (partition-all 3))
        req     (async/chan (async/dropping-buffer 1))
        program (assoc (input-nums 2019 13 ",") 0 2)]
    (run-program! program {:in in :out out :req req})
    {:channels [in out req]
     :data     (atom {:grid {}})}))

(defn input-move [data]
  (let [[bx _] (:ball data)
        [px _] (:paddle data)]
    (compare bx px)))

(defn update-data [data [x y v]]
  (if (= x -1)
    (assoc data :score v)
    (cond-> (update data :grid assoc [x y] v)
      (= v 3) (assoc :paddle [x y])
      (= v 4) (assoc :ball [x y]))))

(defn feed-inputs! [{:keys [channels data]}]
  (let [[in _ req] channels]
    (async/go-loop []
      (when (async/<! req)
        ; output buffer might be buffered and not processed yet.
        ; there's
        (Thread/sleep 30)
        (async/>! in (input-move @data))
        (recur)))))

(defn apply-outputs! [{:keys [channels data]}]
  (let [[_ out _] channels]
    (async/<!!
      (async/reduce (fn [data val]
                      (swap! data update-data val)
                      data)
                    data out))
    (println "SCORE: " (:score @data))))

; pt.2
#_(let [init (init-state)]
  (feed-inputs! init)
  (apply-outputs! init))

(defn setup []
  (let [init (init-state)]
    (feed-inputs! init)
    (async/thread
      (apply-outputs! init))
    init))


#_(let [final-state (->> (iterate next-frame (init-state))
                         (next)
                         (drop-while (fn [{:keys [data]}]
                                       (let [nbricks (aoc.util/countp #(= 2 (second %)) (:grid data))]
                                         (pos? nbricks))))
                         (first))]
    (get-in final-state [:data :score]))


(defn draw [{:keys [data]}]
  (q/background 0)
  (q/no-stroke)
  (let [grid  (:grid @data)
        scale 11]
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
      :setup setup
      :draw draw
      :features [:keep-on-top]
      :middleware [fun-mode]))

  (run-sketch))
