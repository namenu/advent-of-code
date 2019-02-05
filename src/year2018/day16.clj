(ns year2018.day16
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [year2018.device :refer :all]))

(defn sample->candidates [[before [op in1 in2 out] after]]
  (let [candidates (filter #(= (:registers (run-inst before [% in1 in2 out]))
                               (:registers after))
                           opcodes)]
    [op candidates]))

(defn parse-sample [[before inst after]]
  [(device (read-string (subs before 8)))
   (read-string (str "[" inst "]"))
   (device (read-string (subs after 8)))])

(def samples (->> (-> "year2018/day16-1.in" io/resource io/reader line-seq)
                  (remove #(zero? (count %)))
                  (partition 3)
                  (map parse-sample)))

(defn part1 []
  (->> samples
       (map sample->candidates)
       (filter #(>= (count (second %)) 3))
       (count)))


(defn unification [candidates]
  (loop [opcode-map {}
         to-find    (reduce (fn [res [op candidates]]
                              (update res op (fnil set/intersection opcodes) (set candidates)))
                            {}
                            candidates)]
    (let [found      (filter #(= 1 (count (val %))) to-find)
          opcode-map (reduce (fn [m [k v]] (assoc m k (first v))) opcode-map found)
          to-find    (reduce-kv (fn [m op candidates]
                                  (let [v (set/difference candidates (apply set/union (map second found)))]
                                    (if (empty? v)
                                      m
                                      (assoc m op v))))
                                {} to-find)]

      (if (empty? to-find)
        opcode-map
        (recur opcode-map to-find)))))

(def instructions (->> (-> "year2018/day16-2.in" io/resource io/reader line-seq)
                       (map #(read-string (str "[" % "]")))))

(defn part2 []
  (let [candidates (map sample->candidates samples)
        opcode-map (unification candidates)]
    (reduce (fn [m [op & args]]
              (run-inst m (cons (opcode-map op) args)))
            (device [0 0 0 0])
            instructions)))
