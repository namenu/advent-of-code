(ns year2018.day19
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [util :refer [fixed-point]]
            [year2018.day16 :refer [machine load store run-inst]]))


(defn inc-ip [m]
  (assoc m :ip (inc (load m (:ip-reg m)))))

(defn fetch [code m]
  (get code (:ip m)))

(defn parse-inst [s]
  (let [[_ op in1 in2 out] (re-find #"(\w+) (\d+) (\d+) (\d+)" s)
        inst [(keyword op) (Long/parseLong in1) (Long/parseLong in2) (Long/parseLong out)]]
    inst))

(def input (str/split-lines "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5"))
(def input (->> (-> "day19.in" io/resource io/reader line-seq)))
(def code (mapv parse-inst (next input)))

(defn exec [m]
  (if (:halt m)
    m
    (if-let [inst (fetch code m)]
      (-> m
          (store (:ip-reg m) (:ip m))
          (run-inst inst)
          (inc-ip))
      (assoc m :halt true))))

;part1
(-> (fixed-point exec (machine [0 0 0 0 0 0] 3))
    (load 0))


;part2
(let [n (let [m (->> (machine [1 0 0 0 0 0] 3)
                     (iterate exec)
                     (drop 17)
                     first)]
          (load m 4))]
  ; reverse engineered ver.
  #_(reduce + (for [i (range 1 (inc n))
                    j (range 1 (inc n))
                    :when (= (* i j) n)]
                i))

  ; which is meant to be...
  (reduce + (for [i (range 1 (+ n 1))
                  :when (zero? (mod n i))]
              i)))
