;; --- Day 7: Recursive Circus ---
(ns year2017.day07
  (:require [util :refer [find-first unique-one]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def input "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)")
(def input (-> "year2017/day07.in" io/resource io/reader slurp))

(defn parse-line [s]
  (let [[_ name weight] (re-find #"(\w+) \((\d+)\)" s)]
    [name {:weight   (Integer/parseInt weight)
           :holdings (if (str/includes? s "->")
                       (-> (re-find #".* -> (.*)" s)
                           (second)
                           (str/split #", "))
                       [])}]))

(defn input->towers [input]
  (->> (str/split-lines input)
       (map parse-line)
       (into {})))

(defn parent-map [towers]
  (reduce-kv (fn [pairs name t]
               (reduce #(assoc %1 %2 name) pairs (:holdings t)))
             {}
             towers))

(defn find-root [parents]
  (letfn [(iter [t]
            (if-let [p (parents t)]
              (recur p)
              t))]
    (iter (-> parents first first))))

; pt.1
(let [towers (input->towers input)]
  (find-root (parent-map towers)))

(defn tower-weight [name towers]
  (let [t (towers name)]
    (apply + (:weight t) (map #(tower-weight % towers) (:holdings t)))))

(defn find-unbalanced [name towers]
  (if-let [u (unique-one #(tower-weight % towers) (:holdings (towers name)))]
    (recur u towers)
    name))

; pt.2
(let [towers  (input->towers input)
      parents (parent-map towers)

      node    (find-unbalanced (find-root parents) towers)
      sibling (find-first #(not= node %) (:holdings (towers (parents node))))]
  (- (:weight (towers node))
     (- (tower-weight node towers) (tower-weight sibling towers))))