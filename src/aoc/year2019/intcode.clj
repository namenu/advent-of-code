(ns aoc.year2019.intcode
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defn ->machine [program]
  {:program (into {} (map-indexed vector program))
   :ip      0
   :base    0
   :input   PersistentQueue/EMPTY
   :output  []
   :status  :running})

(defn input->state [input]
  (let [numbers (-> input str/trim (str/split #","))
        program (mapv #(Long/parseLong %) numbers)]
    (->machine program)))

(defn binary-op [program op i1 i2 o]
  (assoc program o (op i1 i2)))

(defn add-input [state value]
  (update state :input conj value))

; doesn't work for :input-fn mode
(defn has-input? [state]
  (some? (peek (:input state))))

(defn load-input [state addr]
  (if-let [f (:input-fn state)]
    (let [val (f)]
      (-> state
          (update :program assoc addr val)))

    (let [val (peek (:input state))]
      (-> state
          (update :input pop)
          (update :program assoc addr val)))))

(defn print-output [state output]
  (if-let [f (:output-fn state)]
    (do (f output) state)
    (update state :output conj output)))

(defn jump-if [state pred p1 p2]
  (cond-> state
          (pred p1) (assoc :ip p2)))

(def less-than #(if (< %1 %2) 1 0))

(def equals #(if (= %1 %2) 1 0))

(defn decode [v]
  (let [opcode (rem v 100)
        m      (quot v 100)
        modes  [(rem m 10) (-> m (quot 10) (rem 10)) (-> m (quot 100) (rem 10))]]
    [opcode modes]))

(defn inst-size [opcode]
  ({1 4, 2 4, 3 2, 4 2, 5 3, 6 3, 7 4, 8 4, 9 2, 99 1} opcode))

(defn run [{:keys [program base ip] :as state0}]
  (let [[opcode modes] (decode (get program ip))
        params (mapv program (range (+ ip 1) (+ ip (inst-size opcode))))
        p-in   (fn [p]
                 (let [read #(or (get program %) 0)]
                   (case (modes p)
                     0 (read (params p))
                     1 (params p)
                     2 (read (+ (params p) base)))))
        p-out  (fn [p]
                 (if (= 2 (modes p))
                   (+ (params p) base)
                   (params p)))
        ; move ip first
        state  (update state0 :ip + (inst-size opcode))
        ]
    (case opcode
      ; add (4)
      1 (update state :program binary-op + (p-in 0) (p-in 1) (p-out 2))

      ; mult (4)
      2 (update state :program binary-op * (p-in 0) (p-in 1) (p-out 2))

      ; input (2)
      3 (if (has-input? state)
          (-> (load-input state (p-out 0))
              (assoc :status :running))
          (assoc state0 :status :pause))

      ; output (2)
      4 (print-output state (p-in 0))

      ; jump-if-true (3)
      5 (jump-if state (complement zero?) (p-in 0) (p-in 1))

      ; jump-if-false (3)
      6 (jump-if state zero? (p-in 0) (p-in 1))

      ; less-than (4)
      7 (update state :program binary-op less-than (p-in 0) (p-in 1) (p-out 2))

      ; equals (4)
      8 (update state :program binary-op equals (p-in 0) (p-in 1) (p-out 2))

      ; adjust-base (2)
      9 (update state :base + (p-in 0))

      99 (assoc state :status :halt))))

(defn halted? [state] (= :halt (:status state)))
(defn running? [state] (= :running (:status state)))

(defn run* [state0]
  (->> (iterate run state0)
       (next)
       (drop-while running?)
       (first)))
