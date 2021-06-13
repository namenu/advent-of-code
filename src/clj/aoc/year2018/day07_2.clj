(ns aoc.year2018.day07-2
  (:require [aoc.util :refer [input-lines find-first iterate-while]]
            [aoc.graph :as g]))

;; graph data structures
;;  edges :: #{[a b] [a c] [b d]}
;;  vertices :: #{a b c d}}
(deftype Graph [es vs]
  g/IDirectedGraph
  (edges [_] es)
  (vertices [_] vs)
  (remove-vertex [_ v]
    (Graph. (set (remove #(= (first %) v) es))
            (disj vs v))))

(defn edges->graph [edges]
  (Graph. (set edges)
          (reduce #(apply conj %1 %2) #{} edges)))


(defn init-state
  ([g]
   (init-state g 1 0))
  ([g num-workers step-durations]
   (let [workload (fn [task] (+ (- (int task) 64)
                                step-durations))]
     {:graph       g

      ;; tasks :: { task-name -> workload }
      :tasks       (->> (g/vertices g)
                        (map (juxt identity workload))
                        (into {}))

      :num-workers num-workers

      :doing       #{}
      :done        []
      :tick        0})))

(defn next-task
  "1. 위상적으로 우선이면서
   2. 이미 assign 된 것들을 제외하고
   3. 알파벳 순으로 가장 빠른 것을 선택 (없으면 nil)"
  [state]
  (let [available-tasks (->> (g/topological-tops (:graph state))
                             (remove (:doing state)))]
    (if (seq available-tasks)
      ;; min은 number만 비교할 수 있음. 정점을 char->int로 변환해서 비교
      (apply min-key int available-tasks))))

(defn assign-task
  [state task]
  (:graph state)
  (-> state
      (update :doing conj task)))

(defn finish-task
  [state task]
  (-> state
      (update :graph g/remove-vertex task)
      (update :tasks dissoc task)
      (update :doing disj task)
      (update :done conj task)))

(defn has-todo? [state]
  (some (fn [[_ workload]]
          (pos? workload))
        (:tasks state)))


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
        final (->> (init-state g)
                   (iterate step)
                   (find-first (complement has-todo?)))]
    (apply str (:done final))))

(defn assign-one
  "idle worker가 있고, 일감이 있으면 => 일감 하나 할당 (실패시 nil)"
  [s]
  (if (< (count (:doing s)) (:num-workers s))
    (if-let [t (next-task s)]
      (assign-task s t))))

(defn assign-all [s]
  (iterate-while some? assign-one s))

(defn proceed [s]
  (let [tasks (reduce (fn [ts t]
                        (update ts t dec))
                      (:tasks s)
                      (:doing s))]
    (-> s
        (assoc :tasks tasks)
        (update :tick inc))))

(defn cleanup [s]
  (let [finished-tasks (keep (fn [[task workload]]
                               (if (zero? workload)
                                 task))
                             (:tasks s))]
    (reduce #(finish-task %1 %2) s finished-tasks)))

(defn part2 [lines num-workers step-durations]
  (let [g     (edges->graph (map parse lines))
        step  (fn [s]
                (-> s
                    assign-all
                    proceed
                    cleanup))

        final (->> (init-state g num-workers step-durations)
                   (iterate step)
                   (find-first (complement has-todo?)))]
    (:tick final)))

(let [input ["Step C must be finished before step A can begin."
             "Step C must be finished before step F can begin."
             "Step A must be finished before step B can begin."
             "Step A must be finished before step D can begin."
             "Step B must be finished before step E can begin."
             "Step D must be finished before step E can begin."
             "Step F must be finished before step E can begin."]]
  (assert (= (part1 input) "CABDFE"))


  ;; part2
  (part2 input 2 0)

  (part2 (input-lines 2018 7) 5 60)

  )