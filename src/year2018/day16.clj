(ns year2018.day16
  (:require [clojure.java.io :as io]))


(defn machine [registers]
  {:registers registers})

(defn load [m r]
  (nth (:registers m) r))

(defn store [m r v]
  (update m :registers assoc r v))

(def opcodes [:addr :addi :mulr :muli :banr :bani :borr :bori :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr])

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

(defn num-candidates [[before [_ in1 in2 out] after]]
  (->> (map #(run-inst before [% in1 in2 out]) opcodes)
       (filter #(= (:registers after) (:registers %)))
       (count)))

(def input (-> "day16-1.in" io/resource io/reader line-seq))

(defn parse-sample [[before inst after]]
  [(machine (read-string (subs before 8)))
   (read-string (str "[" inst "]"))
   (machine (read-string (subs after 8)))])

@(def samples
  (->> input
       (remove #(zero? (count %)))
       (partition 3)
       (map parse-sample)
       (map num-candidates)
       (filter #(>= % 3))
       (count)))
