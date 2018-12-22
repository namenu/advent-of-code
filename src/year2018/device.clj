(ns year2018.device
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as str]))

(defn device
  ([registers]
   {:registers registers})
  ([registers ip-reg code]
   {:registers registers
    :ip-reg    ip-reg
    :ip        0
    :code      code}))

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

(defn inc-ip [m]
  (assoc m :ip (inc (load m (:ip-reg m)))))

(defn fetch [{:keys [code ip]}]
  (get code ip))

(defn exec [{:keys [halt ip-reg ip] :as m}]
  (if halt
    m
    (if-let [inst (fetch m)]
      (-> m
          (store ip-reg ip)
          (run-inst inst)
          (inc-ip))
      (assoc m :halt true))))

(defn parse-inst [s]
  (let [[_ op in1 in2 out] (re-find #"(\w+) (\d+) (\d+) (\d+)" s)
        inst [(keyword op) (Integer/parseInt in1) (Integer/parseInt in2) (Integer/parseInt out)]]
    inst))

(defn input->device [input]
  (let [input  (str/split-lines input)
        ip-reg (-> (first input)
                   (subs 4)
                   (Integer/parseInt))
        code   (->> input
                    (next)
                    (mapv parse-inst))]
    (device [0 0 0 0 0 0] ip-reg code)))
