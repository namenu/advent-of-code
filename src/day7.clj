(ns day7)

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
        (let [comparator (fn [x y]
                           (let [gx (count (deps x))
                                 gy (count (deps y))]
                             (if (= gx gy)
                               (< (int x) (int y))
                               (< gx gy))))
              nodes      (apply sorted-set-by comparator todo)
              top        (first nodes)]
          (recur (disj nodes top)
                 (remove-node deps top)
                 (str result top)))))))


(defn worker-available? [workers]
  (< (count workers) *num-workers*))

(defn schedule [workers task cur-time]
  (let [duration (+ (- (int task) 64) *step-duration*)]
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
          (let [comparator   (fn [x y]
                               (let [gx (count (deps x))
                                     gy (count (deps y))]
                                 (if (= gx gy)
                                   (< (int x) (int y))
                                   (< gx gy))))
                top          (first (apply sorted-set-by comparator todo))

                schedulable? (empty? (deps top))]
            (if (and schedulable? (worker-available? workers))
              (recur elapsed
                     (schedule workers top elapsed)
                     (disj todo top)
                     deps)
              (let [[[task time] workers] (wait-till-available workers)]
                (recur time workers todo (remove-node deps task))))))))))


;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test-day5
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
    (let [input (->> "day7.in" io/resource io/reader line-seq)]))
  )

(run-tests)