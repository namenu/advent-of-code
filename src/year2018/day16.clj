(ns year2018.day16
  (:refer-clojure :exclude [load])
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))


(defn machine
  ([registers]
   {:registers registers})
  ([registers ip-reg]
   {:registers registers
    :ip-reg    ip-reg
    :ip        0}))

(defn load [m r]
  (nth (:registers m) r))

(defn store [m r v]
  (update m :registers assoc r v))

(def opcodes #{:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr})

(defn run-inst [m [op in1 in2 out]]
  (let [v (case op
            :addr (+ (load m in1) (load m in2))
            :addi (+ (load m in1) in2)
            :mulr (* (load m in1) (load m in2))
            :muli (* (load m in1) in2)
            :banr (bit-and (load m in1) (load m in2))
            :bani (bit-and (load m in1) in2)
            :borr (bit-or (load m in1) (load m in2))
            :bori (bit-or (load m in1) in2)
            :setr (load m in1)
            :seti in1
            :gtir (if (> in1 (load m in2)) 1 0)
            :gtri (if (> (load m in1) in2) 1 0)
            :gtrr (if (> (load m in1) (load m in2)) 1 0)
            :eqir (if (= in1 (load m in2)) 1 0)
            :eqri (if (= (load m in1) in2) 1 0)
            :eqrr (if (= (load m in1) (load m in2)) 1 0))]
    (store m out v)))

(defn sample->candidates [[before [op in1 in2 out] after]]
  (let [candidates (filter #(= (:registers (run-inst before [% in1 in2 out]))
                               (:registers after))
                           opcodes)]
    [op candidates]))

(defn parse-sample [[before inst after]]
  [(machine (read-string (subs before 8)))
   (read-string (str "[" inst "]"))
   (machine (read-string (subs after 8)))])

(def samples (->> (-> "day16-1.in" io/resource io/reader line-seq)
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

(def instructions (->> (-> "day16-2.in" io/resource io/reader line-seq)
                       (map #(read-string (str "[" % "]")))))

(defn part2 []
  (let [candidates (map sample->candidates samples)
        opcode-map (unification candidates)]
    (reduce (fn [m [op & args]]
              (run-inst m (cons (opcode-map op) args)))
            (machine [0 0 0 0])
            instructions)))
