;; --- Day 5: Sunny with a Chance of Asteroids ---
(ns aoc.year2019.day05
  (:require [aoc.util :refer [input-nums]]
            [clojure.core.async :as async]
            [aoc.year2019.intcode :refer :all]))

(defn from-chan [ch]
  (take-while identity (repeatedly #(async/<!! ch))))

(defn diagnostic-code [program input]
  (let [in  (async/chan)
        out (async/chan)]
    (run-program program in out)
    (async/>!! in input)
    (last (from-chan out))))

(comment
  (let [program (input-nums 2019 5 ",")]
    ; pt.1
    (prn (diagnostic-code program 1))

    ; pt.2
    (prn (diagnostic-code program 5))))
