;; --- Day 7: Amplification Circuit ---
(ns aoc.year2019.day07
  (:require [aoc.util :refer [input find-first]]
            [aoc.year2019.intcode :refer :all]
            [clojure.core.async :as async]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn input->program [input]
  (let [numbers (-> input str/trim (str/split #","))]
    (mapv #(Integer/parseInt %) numbers)))

(defn make-amps [program phases]
  (let [[a b c d e] (map #(let [c (async/chan 1)]
                            (async/>!! c %)
                            c)
                         phases)
        f (async/chan)]
    (run-program program a b "A")
    (run-program program b c "B")
    (run-program program c d "C")
    (run-program program d e "D")
    (run-program program e f "E")
    [a f]))

(defn amplify-0 [[amps-in amps-out]]
  (async/>!! amps-in 0)
  (async/<!! amps-out))

(defn amplify-fb [[amps-in amps-out]]
  (loop [feedback 0]
    (if-let [_ (async/>!! amps-in feedback)]
      (recur (async/<!! amps-out))
      feedback)))

(defn max-thruster [program amplifier phase-range]
  (->> (for [phases (combo/permutations phase-range)
             :let [amps (make-amps program phases)]]
         (amplifier amps))
       (apply max,,,)))

(comment
  (let [program (input->program (input 2019 7))]
    ; pt.1
    (prn (max-thruster program amplify-0 [0 1 2 3 4]))
    ; pt.2
    (prn (max-thruster program amplify-fb [9 7 8 5 6]))
    ))
