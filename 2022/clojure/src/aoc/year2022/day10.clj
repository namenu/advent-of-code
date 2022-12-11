(ns aoc.year2022.day10
  (:require [clojure.string :as str])
  (:import (java.util Collections)))

(def input "noop\naddx 3\naddx -5\n\n")

(defn parse-inst [s]
  (let [[i x] (str/split s #" ")]
    (case i
      "noop" [:noop]
      "addx" [:addx (parse-long x)])))

(defrecord State [cycle x])

(def state0 (->State 1 1))

(defmulti exec (fn [_state inst] (first inst)))

(defmethod exec :noop [state _]
  (update state :cycle + 1))

(defmethod exec :addx [state [_ v]]
  (-> state
      (update :cycle + 2)
      (update :x + v)))

(def input "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop")

@(def input (slurp (clojure.java.io/resource "day10.in")))


(let [program     (->> (str/split-lines input)
                       (map parse-inst))

      states      (vec (reductions exec state0 program))

      x-at        (let [cycles (->> states (mapv :cycle))]
                    (fn [cycle]
                      (let [i (Collections/binarySearch cycles cycle)]
                        (get states (if (neg? i)
                                      (- (- i) 2)
                                      i)))))

      strength-at (fn [cycle]
                    (* cycle (:x (x-at cycle))))]

  (->> [(strength-at 20)
        (strength-at 60)
        (strength-at 100)
        (strength-at 140)
        (strength-at 180)
        (strength-at 220)]
       (apply +))
  )

(comment
  (Collections/binarySearch [1 3 5 7 9] 5 compare))


;; pt. 2

(defn render [{:keys [cycle x]}]
  (let [[c r] ((juxt #(rem % 40) #(quot % 40)) (dec cycle))
        pixel (if (<= (dec x) c (inc x)) \# \.)]
    [[c r] pixel]))

(defn interpolate [[s1 s2]]
  (for [cycle (range (:cycle s1) (:cycle s2))]
    (->State cycle (:x s1))))

(require '[aoc.grid :as g])

(let [program (->> (str/split-lines input)
                   (map parse-inst))

      states  (->> (reductions exec state0 program)
                   (partition 2 1)
                   (mapcat interpolate))]

  (let [crt (map render states)]
    (g/print-grid (into {} crt))))