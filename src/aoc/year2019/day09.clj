;; --- Day 9: Sensor Boost ---
(ns aoc.year2019.day09
  (:require [aoc.util :refer [input-nums]]
            [aoc.year2019.intcode :refer :all]
            [clojure.core.async :as async]))

(defn run-with-mode [program mode]
  (let [in  (async/chan 1)
        out (async/chan)]
    (async/>!! in mode)
    (run-program program in out)
    (async/<!! out)))

(comment
  (let [program (input-nums 2019 9 ",")]
    ; pt.1
    (prn (run-with-mode program 1))

    ; pt.2
    (prn (run-with-mode program 2))))
