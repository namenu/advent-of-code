;; --- Day 2: 1202 Program Alarm ---
(ns aoc.year2019.day02
  (:require [aoc.util :refer [input find-first]]
            [clojure.string :as str]
            [aoc.year2019.intcode :refer :all]))

(defn input->program [input]
  (let [numbers (-> input str/trim (str/split #","))]
    (mapv #(Integer/parseInt %) numbers)))

(defn part1 [program noun verb]
  (let [init (->machine (assoc program 1 noun 2 verb))]
    (-> (run init)
        (get-in [:program 0]))))

(defn part2 [program output]
  ;; pmap ver.
  #_(->> (for [n (range 100)
               v (range 100)]
           [n v])
         (pmap #(vector % (apply part1 program %)))
         (filter #(= (second %) output)))
  (for [n (range 100)
        v (range 100)
        :when (= (part1 program n v) output)]
    (+ (* 100 n) v)))


(let [program (input->program (input 2019 2))]
  ; pt.1
  #_(part1 program 12 1)

  ; pt.2
  (part2 program 19690720)
  )
