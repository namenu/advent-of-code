(ns year2019.day09-test
  (:require [year2019.intcode :refer :all]
            [clojure.test :refer :all]))

(defn program->output [program]
  (-> (->machine program) (run*) :output))

(deftest part1
  (let [program [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]]
    (is (= program (program->output program))))
  (let [program [1102, 34915192, 34915192, 7, 4, 7, 99, 0]]
    (is (= [1219070632396864] (program->output program))))
  (let [program [104, 1125899906842624, 99]]
    (is (= [1125899906842624] (program->output program))))
  )
