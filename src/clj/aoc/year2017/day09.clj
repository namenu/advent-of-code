;; --- Day 9: Stream Processing ---
(ns aoc.year2017.day09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def init-state
  {:depth   0
   :mode    :stream
   :score   0
   :garbage 0})

(defn transit [state c]
  (case (:mode state)
    :stream
    (case c
      \{ (update state :depth inc)
      \} (-> state
             (update :depth dec)
             (update :score + (:depth state)))
      \, state
      \< (assoc state :mode :garbage))

    :garbage
    (case c
      \> (assoc state :mode :stream)
      \! (assoc state :mode :ignore)
      (update state :garbage inc))

    :ignore
    (assoc state :mode :garbage)))

(defn process-stream [stream]
  (reduce transit init-state stream))

(let [input (->> "year2017/day09.in" io/resource slurp str/trim)]
  (process-stream input))


;; tests
(require '[clojure.test :refer [deftest testing is are run-tests]])

(deftest test
  (testing "pt.1"
    (are [expected actual] (= expected (:score actual))
      1 (process-stream "{}")
      6 (process-stream "{{{}}}")
      5 (process-stream "{{},{}}")
      16 (process-stream "{{{},{},{{}}}}")
      1 (process-stream "{<a>,<a>,<a>,<a>}")
      9 (process-stream "{{<ab>},{<ab>},{<ab>},{<ab>}}")
      9 (process-stream "{{<!!>},{<!!>},{<!!>},{<!!>}}")
      3 (process-stream "{{<a!>},{<a!>},{<a!>},{<ab>}}")))

  (testing "pt.2"
    (are [expected actual] (= expected (:garbage actual))
      0 (process-stream "<>")
      17 (process-stream "<random characters>")
      3 (process-stream "<<<<>")
      2 (process-stream "<{!>}>")
      0 (process-stream "<!!>")
      0 (process-stream "<!!!>>")
      10 (process-stream "<{o\"i!a,<{i<a>"))))

(run-tests)
