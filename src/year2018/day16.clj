(ns year2018.day16
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))


(defn machine [registers]
  {:registers registers})

(defn load [m r]
  (nth (:registers m) r))

(defn store [m r v]
  (update m :registers assoc r v))

(def opcodes #{:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr})
(set/intersection (set opcodes) [:addr :muli])

(defn run-inst [m [op in1 in2 out]]
  (case op
    :addr
    (store m out (+ (load m in1) (load m in2)))
    :addi
    (store m out (+ (load m in1) in2))

    :mulr
    (store m out (* (load m in1) (load m in2)))
    :muli
    (store m out (* (load m in1) in2))

    :banr
    (store m out (bit-and (load m in1) (load m in2)))
    :bani
    (store m out (bit-and (load m in1) in2))

    :borr
    (store m out (bit-or (load m in1) (load m in2)))
    :bori
    (store m out (bit-or (load m in1) in2))

    :setr
    (store m out (load m in1))
    :seti
    (store m out in1)

    :gtir
    (store m out (if (> in1 (load m in2)) 1 0))
    :gtri
    (store m out (if (> (load m in1) in2) 1 0))
    :gtrr
    (store m out (if (> (load m in1) (load m in2)) 1 0))

    :eqir
    (store m out (if (= in1 (load m in2)) 1 0))
    :eqri
    (store m out (if (= (load m in1) in2) 1 0))
    :eqrr
    (store m out (if (= (load m in1) (load m in2)) 1 0))
    ))

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

@(def part1
   (->> samples
        (map sample->candidates)
        (filter #(>= (count (second %)) 3))
        (count)))


(def opcode-map {0  :eqir
                 1  :seti
                 2  :eqri
                 3  :eqrr
                 4  :addi
                 5  :setr
                 6  :gtrr
                 7  :gtri
                 8  :muli
                 9  :bori
                 10 :bani
                 11 :borr
                 12 :gtir
                 13 :banr
                 14 :addr
                 15 :mulr})

(def init-map (zipmap (range 16) (repeat opcodes)))

(->> samples
     (map sample->candidates)
     (reduce (fn [res [op candidates]]
               (update res op set/intersection (set candidates)))
             init-map)
     ; manually repeat eval with updated opcode-map
     (map (fn [[op candidates]]
            [op (set/difference candidates (set (vals opcode-map)))]))
     (remove #(empty? (second %))))

@(def instructions (->> (-> "day16-2.in" io/resource io/reader line-seq)
                        (map #(read-string (str "[" % "]")))))

(def part2 (reduce (fn [m [op & args]]
                     (run-inst m (cons (opcode-map op) args)))
                   (machine [0 0 0 0])
                   instructions
                   ))
