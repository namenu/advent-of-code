;; --- Day 8: I Heard You Like Registers ---
(ns aoc.year2017.day08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defmacro inst->expr [reg op value lhs pred rhs]
  (let [m (gensym)]
    `(fn [~m]
       (if (~pred (~lhs ~m 0) ~rhs)
         (update ~m ~reg (fnil ~op 0) ~value)
         ~m))))

(defn parse-inst [s]
  (let [[reg op value _ lhs pred rhs] (str/split s #"\s")]
    (inst->expr (keyword reg)
                ({"inc" +, "dec" -} op)
                (Integer/parseInt value)
                (keyword lhs)
                ({">" >, "<" <, ">=" >=, "<=" <=, "==" =, "!=" not=} pred)
                (Integer/parseInt rhs))))


(def input "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10")
(def input (-> "year2017/day08.in" io/resource slurp))

(let [instructions (->> (str/split-lines input)
                        (map parse-inst))]

  ; pt.1
  (->> (reduce #(%2 %1) {} instructions)
       (apply max-key val))

  ; pt.2
  (->> (reductions #(%2 %1) {} instructions)
       (drop 1)
       (map #(apply max-key val %))
       (apply max-key second)))
