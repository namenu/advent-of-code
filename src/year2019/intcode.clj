(ns year2019.intcode
  (:require [util :refer [find-first]]))

(defn ->machine [program]
  {:program program
   :ip      0
   :input   []
   :output  []})

(defn binary-op [program op i1 i2 o]
  (assoc program o (op i1 i2)))

(defn store [program addr value]
  (assoc program addr value))

(defn jump-if [state pred p1 p2]
  (cond-> state
          (pred p1) (assoc :ip p2)))

(def less-than #(if (< %1 %2) 1 0))

(def equals #(if (= %1 %2) 1 0))

(def ^:dynamic *input* 1)

(defn decode [v]
  (let [opcode (rem v 100)
        m      (quot v 100)
        modes  [(rem m 10) (-> m (quot 10) (rem 10)) (-> m (quot 100) (rem 10))]]
    [opcode modes]))

(defn inst-size [opcode]
  ({1 4, 2 4, 3 2, 4 2, 5 3, 6 3, 7 4, 8 4, 99 1} opcode))

(defn get-param [program param mode]
  (if (zero? mode)
    (get program param)
    param))

(defn run [{:keys [program ip] :as state0}]
  (let [[opcode modes] (decode (get program ip))
        params  (subvec program (+ ip 1) (+ ip (inst-size opcode)))
        mparams (mapv #(get-param program %1 %2) params modes)

        ; move ip first
        state   (update state0 :ip + (inst-size opcode))
        ]
    (case opcode
      ; add (4)
      1 (update state :program binary-op + (mparams 0) (mparams 1) (params 2))

      ; mult (4)
      2 (update state :program binary-op * (mparams 0) (mparams 1) (params 2))

      ; read (2)
      3 (let [in *input*]
          (update state :program store (params 0) in))

      ; print (2)
      4 (let [output (mparams 0)]
          (update state :output conj output))

      ; jump-if-true (3)
      5 (jump-if state (complement zero?) (mparams 0) (mparams 1))

      ; jump-if-false (3)
      6 (jump-if state zero? (mparams 0) (mparams 1))

      ; less-than (4)
      7 (update state :program binary-op less-than (mparams 0) (mparams 1) (params 2))

      ; equals (4)
      8 (update state :program binary-op equals (mparams 0) (mparams 1) (params 2))

      99 (do
           (prn "HALT")
           (assoc state :halt true)))))

(defn halted? [{:keys [program ip]}]
  (= 99 (get program ip)))

(defn run* [state0]
  (->> (iterate run state0)
       (find-first halted?)))

(comment
  (binding [*input* 8]
    (let [program [3, 3, 1108, -1, 8, 3, 4, 3, 99]
          state   (run* (->machine program))]
      state)))