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

(defn make-amp [program phase]
  (let [in  (async/chan)
        out (async/chan)]
    (run-program program in out)
    (async/>!! in phase)
    [in out]))

(defn make-amps [program phases]
  (let [[a b c d e f] (repeatedly async/chan 6)]
    ))

(defn amplify [amps]
  (reduce (fn [input [amp-in amp-out]]
            (let [_ (async/>!! amp-in input)]
              (async/<!! amp-out)))
          0
          amps))

(defn max-thruster [program amplifier phases]
  (->> (for [phases (combo/permutations phases)
             :let [amps (mapv make-amp (repeat 5 program) phases)]]
         (amplifier amps))
       (apply max,,,)))

(defn amplify-fb [amps]
  (reduce (fn [input [amp-in amp-out]]
            (if-let [_ (async/>!! amp-in input)]
              (async/<!! amp-out)
              (reduced input)))
          0
          (cycle amps)))

(comment
  (let [program (input->program (input 2019 7))]
    ; pt.1
    (prn (max-thruster program amplify [0 1 2 3 4]))
    ; pt.2
    (prn (max-thruster program amplify-fb [9 7 8 5 6]))
    ))
