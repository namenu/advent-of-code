(ns namenu.aoc-2021
  (:require [clojure.string :as str]))

(def input "199\n200\n208\n210\n200\n207\n240\n269\n260\n263")

(def input (slurp "resources/input.1"))

(def nums
  (map parse-long (str/split-lines input)))

(defn num-increasing [nums]
  (->> nums
       (partition 2 1)
       (filter #(apply < %))
       count))

(time
  (->> nums
       (partition 3 1)
       (map #(apply + %))
       num-increasing))


;;

(defn increasing [nums]
  (apply < nums))

(->> (partition 2 1 nums)
     (filter increasing)
     count)

(time
  (->> (map #(increasing [%1 %2]) nums (drop 3 nums))
       (filter true?)
       count))