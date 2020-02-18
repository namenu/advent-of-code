(ns aoc.year2019.intcode
  (:require [clojure.core.async :as async]))

(defn intcode [program]
  {:memory (into {} (map-indexed vector program))
   :ip     0
   :base   0
   :status :runnable})

(defn binary-op [memory op i1 i2 o]
  (assoc memory o (op i1 i2)))

#_(defn add-input
    "possible opts => :ascii"
    [state value & [opts]]
    (if (:ascii opts)
      (update state :input into (map int value))
      (update state :input conj value)))

(defn read-input [{:keys [in-ch req-ch] :as state} addr]
  (when-not in-ch
    (throw (ex-info "program reads input while input channel is unset." state)))

  (async/>!! req-ch :read-input)
  (let [val (async/<!! in-ch)]
    (update state :memory assoc addr val))

  #_(if-let [f (:input-fn state)]
      (let [val (f)]
        (-> state
            (update :memory assoc addr val)))

      (let [val (peek (:input state))]
        (-> state
            (update :input pop)
            (update :memory assoc addr val)))))

(defn write-output [{:keys [out-ch] :as state} output]
  (when-not out-ch
    (throw (ex-info "program writes output while output channel is unset." state)))

  (async/>!! out-ch output)
  state)

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

(defn halt [{:keys [in-ch out-ch req-ch] :as state}]
  (when in-ch (async/close! in-ch))
  (when out-ch (async/close! out-ch))
  (when req-ch (async/close! req-ch))
  (assoc state :status :halt))

(defn inst-size [opcode]
  ({1 4, 2 4, 3 2, 4 2, 5 3, 6 3, 7 4, 8 4, 9 2, 99 1} opcode))

(defn instruction-cycle [{:keys [memory base ip] :as state0}]
  (let [[opcode modes] (decode (get memory ip))
        params (mapv memory (range (+ ip 1) (+ ip (inst-size opcode))))
        p-in   (fn [p]
                 (let [read #(or (get memory %) 0)]
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
      1 (update state :memory binary-op + (p-in 0) (p-in 1) (p-out 2))

      ; mult (4)
      2 (update state :memory binary-op * (p-in 0) (p-in 1) (p-out 2))

      ; input (2)
      3 (read-input state (p-out 0))

      ; output (2)
      4 (write-output state (p-in 0))

      ; jump-if-true (3)
      5 (jump-if state (complement zero?) (p-in 0) (p-in 1))

      ; jump-if-false (3)
      6 (jump-if state zero? (p-in 0) (p-in 1))

      ; less-than (4)
      7 (update state :memory binary-op less-than (p-in 0) (p-in 1) (p-out 2))

      ; equals (4)
      8 (update state :memory binary-op equals (p-in 0) (p-in 1) (p-out 2))

      ; adjust-base (2)
      9 (update state :base + (p-in 0))

      99 (halt state))))

(defn halted? [state] (= :halt (:status state)))

(defn run-program! [program & [{:keys [in out req]
                                :or   {req (async/chan (async/dropping-buffer 1))}}]]
  (let [state0 (merge (intcode program)
                      {:in-ch  in
                       :out-ch out
                       :req-ch req})]
    (async/thread
      (->> (iterate instruction-cycle state0)
           (drop-while (complement halted?))
           (first)))))

(defn run-pure-program [program & args]
  (let [in  (async/chan)
        out (async/chan)]
    (run-program! program {:in in :out out})
    (async/onto-chan in args)
    (take-while identity (repeatedly #(async/<!! out)))))
