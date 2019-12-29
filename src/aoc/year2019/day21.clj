;; --- Day 21: Springdroid Adventure ---
(ns aoc.year2019.day21
  (:require [aoc.util :refer [input]]
            [aoc.year2019.intcode :refer :all]
            [clojure.string :as str]))

(defn walk-or-run [robot cmd]
  (let [opts   {:ascii true}
        robot  (-> robot (add-input cmd opts) (run))
        output (:output robot)]
    (if (= (peek output) 10)
      (println (get-output robot opts))
      (peek output))))

(let [robot (input->machine (input 2019 21))]
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
        robot (input->machine (input 2019 21))]
    (walk-or-run robot cmd)))
