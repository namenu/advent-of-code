;; --- Day 2: 1202 Program Alarm ---
(ns year2019.day02
  (:require [util :refer [input find-first]]
            [clojure.string :as str]))

(defn input->program [input]
  (let [numbers (-> input str/trim (str/split #","))]
    (mapv #(Integer/parseInt %) numbers)))

(defn exec [program op i1 i2 o]
  (let [v1 (get program i1)
        v2 (get program i2)]
    (assoc program o (op v1 v2))))

(defn run [{:keys [program ip] :as state}]
  (let [op (get program ip)]
    (case op
      1 (let [[i1 i2 o] (subvec program (+ ip 1) (+ ip 4))]
          (-> state
              (update :program exec + i1 i2 o)
              (update :ip + 4)))

      2 (let [[i1 i2 o] (subvec program (+ ip 1) (+ ip 4))]
          (-> state
              (update :program exec * i1 i2 o)
              (update :ip + 4)))

      99 state)))

(defn halted? [{:keys [program ip]}]
  (= 99 (get program ip)))

(defn run* [program noun verb]
  (let [init  {:program (assoc program 1 noun 2 verb)
               :ip      0}
        final (->> (iterate run init)
                   (find-first halted?))]
    (get (:program final) 0)))

(defn part2 [program output]
  (for [n (range 100)
        v (range 100)
        :when (= (run* program n v) output)]
    (+ (* 100 n) v)))


(comment
  (let [program (input->program (input 2019 2))]
    ; pt.1
    (run* program 12 2)

    ; pt.2
    (part2 program 19690720)
    ))
