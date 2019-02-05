(ns year2018.day07)

(defn parse [s]
  (let [[_ x y] (re-find #"Step (\S) must be finished before step (\S) can begin." s)]
    [(get x 0) (get y 0)]))

(def ^:dynamic *num-workers* 5)
(def ^:dynamic *step-duration* 60)

(defn data->graph [data]
  (reduce (fn [g [x y]]
            (update g y (fnil conj #{}) x))
          {}
          data))

(defn update-vals
  "Applies f to all the vals in the map"
  [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} (or m {})))

(defn remove-node [graph node]
  (update-vals graph (fn [v] (disj v node))))

(defn part1 [input]
  (let [data  (map parse input)
        nodes (into #{} (flatten data))
        graph (data->graph data)]
    (loop [todo   nodes
           deps   graph
           result ""]
      (if (empty? todo)
        result
        (let [no-deps (filter #(empty? (deps %)) todo)
              task    (first (sort no-deps))]
          (recur (disj todo task)
                 (remove-node deps task)
                 (str result task)))))))

(defn worker-available? [workers]
  (< (count workers) *num-workers*))

(defn char->int ^long [c]
  #?(:clj (int c)
     :cljs (.charCodeAt c 0)))

(defn schedule [workers task cur-time]
  (let [duration (+ (- (char->int task) 64) *step-duration*)]
    (assoc workers task (+ duration cur-time))))

(defn wait-till-available [workers]
  (let [done (apply min-key val workers)]
    [done (dissoc workers (first done))]))

(defn part2 [input num-workers step-duration]
  (binding [*num-workers*   num-workers
            *step-duration* step-duration]
    (let [data  (map parse input)
          nodes (into #{} (flatten data))
          graph (data->graph data)]
      (loop [elapsed 0
             workers {}
             todo    nodes
             deps    graph]
        (if (empty? todo)
          ; wait all workers
          (second (apply max-key val workers))
          (let [no-deps (filter #(empty? (deps %)) todo)
                task    (first (sort no-deps))]
            (if (and task (worker-available? workers))
              (recur elapsed (schedule workers task elapsed) (disj todo task) deps)
              (let [[[task time] workers] (wait-till-available workers)]
                (recur time workers todo (remove-node deps task))))))))))


;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test-day7
  (let [input ["Step C must be finished before step A can begin."
               "Step C must be finished before step F can begin."
               "Step A must be finished before step B can begin."
               "Step A must be finished before step D can begin."
               "Step B must be finished before step E can begin."
               "Step D must be finished before step E can begin."
               "Step F must be finished before step E can begin."]]
    (is (= "CABDFE" (part1 input)))
    (is (= 15 (part2 input 2 0))))

  (comment
    (require '[clojure.java.io :as io])
    (let [input (->> "year2018/day07.in" io/resource io/reader line-seq)]))
  )

(run-tests)