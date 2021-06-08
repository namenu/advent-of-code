(ns aoc.year2018.day07-2
  (:require [aoc.util :refer [input-lines find-first]]))

;; graph data structures
;; ex: {:edges #{[a b] [a c] [b d]}
;;      :vertices #{a b c d}}

(defn edges->graph [edges]
  {:edges    (set edges)
   :vertices (reduce #(apply conj %1 %2) #{} edges)})

(defn remove-vertex [{:keys [edges vertices]} v]
  {:edges    (set (remove #(= (first %) v) edges))
   :vertices (disj vertices v)})

(defn topological-tops [{:keys [edges vertices]}]
  (->> (map second edges)
       (apply disj vertices)))


(defn init-state [g num-workers]
  (let [workload (fn [task] (- (int task) 64))]
    {:graph g

     ;; tasks :: { task-name -> workload }
     :tasks (->> (:vertices g)
                 (map (juxt identity workload))
                 (into {}))

     :doing #{}
     :done  []}))

(defn next-task
  "위상적으로 우선이면서 알파벳 순으로 가장 빠른 것을 선택"
  [state]
  (->> (topological-tops (:graph state))
       ;; min은 number만 비교할 수 있음. 정점을 char->int로 변환해서 비교
       (apply min-key int)))

(defn assign-task
  [state task]
  (:graph state)
  (-> state
      (update :graph remove-vertex task)
      (update :doing conj task)))

(defn finish-task
  [state task]
  (-> state
      (update :tasks dissoc task)
      (update :doing disj task)
      (update :done conj task)))

(defn has-todo? [state]
  (seq (:tasks state)))


(defn parse [s]
  (let [[_ x y] (re-find #"Step (\S) must be finished before step (\S) can begin." s)]
    [(get x 0) (get y 0)]))

(defn part1 [lines]
  (let [g     (edges->graph (map parse lines))
        step  (fn [s]
                (let [t (next-task s)]
                  (-> s
                      (assign-task t)
                      (finish-task t))))
        final (->> (init-state g 1)
                   (iterate step)
                   (find-first (complement has-todo?)))]
    (apply str (:done final))))


(let [input ["Step C must be finished before step A can begin."
             "Step C must be finished before step F can begin."
             "Step A must be finished before step B can begin."
             "Step A must be finished before step D can begin."
             "Step B must be finished before step E can begin."
             "Step D must be finished before step E can begin."
             "Step F must be finished before step E can begin."]]
  (part1 input))