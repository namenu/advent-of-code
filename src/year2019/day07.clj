;; --- Day 7: Amplification Circuit ---
(ns year2019.day07
  (:require [util :refer [input find-first]]
            [year2019.intcode :refer :all]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn input->program [input]
  (let [numbers (-> input str/trim (str/split #","))]
    (mapv #(Integer/parseInt %) numbers)))

(defn make-amp [program phase]
  (add-input (->machine program) phase))

(defn amplify [program phases]
  (loop [[p & ps] phases
         input 0]
    (if p
      (let [init   (add-input (make-amp program p) input)
            output (-> (run* init) :output peek)]
        (recur ps output))
      input)))

(defn max-thruster [program amplifier phases]
  (->> (for [phases (combo/permutations phases)]
         (amplifier program phases))
       (apply max,,,)))

(defn get-output [state]
  (let [output-cnt (count (:output state))]
    (->> (iterate run state)
         (find-first #(or (> (count (:output %)) output-cnt) (halted? %))))))

(defn amplify-fb [program phases]
  (loop [amps  (mapv make-amp (repeat 5 program) phases)
         idx   0
         input 0]
    (let [before (add-input (get amps idx) input)
          after  (get-output before)
          output (peek (:output after))]
      (if (and (= idx 4) (halted? after))
        output

        (recur (assoc amps idx after)
               (rem (inc idx) 5)
               output
               )))))


(let [program (input->program (input 2019 7))]
  ; pt.1
  (max-thruster program amplify [0 1 2 3 4])
  ; pt.2
  (max-thruster program amplify-fb [9 7 8 5 6])
  )