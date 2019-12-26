;; --- Day 21: Springdroid Adventure ---
(ns aoc.year2019.day21
  (:require [aoc.util :refer [input]]
            [aoc.year2019.intcode :refer :all]
            [clojure.string :as str]))

(defn walk-or-run [robot cmd]
  (let [inputs (map int cmd)
        robot  (reduce #(add-input %1 %2) robot inputs)
        output (:output (run* robot))]
    (if (= (peek output) 10)
      (doseq [l (str/split-lines (apply str (map char output)))]
        (println l))
      (peek output))))

(let [robot (input->state (input 2019 21))]
  ; pt.1
  (let [; ¬A∨¬B∨¬C ∧ D
        cmd (str "NOT A J\n"
                 "NOT B T\n"
                 "OR T J\n"
                 "NOT C T\n"
                 "OR T J\n"
                 "AND D J\n"
                 "WALK\n")]
    (walk-or-run robot cmd))

  ; pt.2
  (let [; ¬A∨¬B∨¬C ∧ D ∧ ¬(¬E∧¬H)
        ;=¬A∨¬B∨¬C ∧ D ∧ (E∨H)
        cmd   (str "NOT A J\n"
                   "NOT B T\n"
                   "OR T J\n"
                   "NOT C T\n"
                   "OR T J\n"
                   "AND D J\n"

                   "OR E T\n"
                   "AND E T\n"
                   "OR H T\n"
                   "AND T J\n"
                   "RUN\n")
        robot (input->state (input 2019 21))]
    (walk-or-run robot cmd)))
