(ns day7)

(defn parse [s]
  (let [[_ x y] (re-find #"Step (\S) must be finished before step (\S) can begin." s)]
    [(get x 0) (get y 0)]))


(def input ["Step C must be finished before step A can begin."
            "Step C must be finished before step F can begin."
            "Step A must be finished before step B can begin."
            "Step A must be finished before step D can begin."
            "Step B must be finished before step E can begin."
            "Step D must be finished before step E can begin."
            "Step F must be finished before step E can begin."])
(def num-workers 2)
(def step-duration 0)

(defn data->graph [data]
  (reduce (fn [g [x y]]
            (update g y (fnil conj #{}) x))
          {}
          data))

(defn nodes [graph]
  (into #{} (map key graph)))

(defn update-vals
  "Applies f to all the vals in the map"
  [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} (or m {})))

(defn remove-node [graph node]
  (update-vals graph (fn [v] (disj v node))))

(defn part1 [input]
  (let [data  (map parse input)
        nodes (into #{} (flatten data))
        g     (data->graph data)]
    (loop [nodes  nodes
           g      g
           result ""]
      (if (empty? nodes)
        result
        (let [comparator (fn [x y]
                           (let [gx (count (g x))
                                 gy (count (g y))]
                             (if (= gx gy)
                               (< (int x) (int y))
                               (< gx gy))))
              nodes      (apply sorted-set-by comparator nodes)
              top        (first nodes)]
          (recur (disj nodes top)
                 (remove-node g top)
                 (str result top)))))))



(part1 input)

(def input (->> "day7.in" io/resource io/reader line-seq))
(def num-workers 5)
(def step-duration 60)


(defn unavailable-workers [workers cur-time]
  (filter (fn [[_ v]] (> v cur-time)) workers))

(defn worker-available? [workers]
  (< (count workers) num-workers))

(defn schedule [workers task cur-time]
  (let [duration (+ (- (int task) 64) step-duration)]
    (assoc workers task (+ duration cur-time))))

(defn wait-till-available [workers]
  (let [done (apply min-key val workers)]
    [(first done) (second done)]))


(require '[clojure.java.io :as io])
(def input (->> "day7.in" io/resource io/reader line-seq))

(let [data  (map parse input)
      nodes (into #{} (flatten data))
      g     (data->graph data)]
  (loop [elapsed 0
         workers {}
         todo    nodes
         g       g]
    (prn elapsed todo workers)
    (if (empty? todo)
      elapsed
      (if (worker-available? workers)

        (let [comparator   (fn [x y]
                             (let [gx (count (g x))
                                   gy (count (g y))]
                               (if (= gx gy)
                                 (< (int x) (int y))
                                 (< gx gy))))
              top          (first (apply sorted-set-by comparator todo))

              schedulable? (empty? (g top))]

          (if schedulable?
            (do
              (prn "schedule" top todo)
              (recur elapsed
                     (schedule workers top elapsed)
                     (disj todo top)
                     g))

            (let [[done elapsed] (wait-till-available workers)]
              (recur elapsed
                     (dissoc workers done)
                     todo
                     (remove-node g done)))))

        (let [[done elapsed] (wait-till-available workers)]
          (recur elapsed
                 (dissoc workers done)
                 todo
                 (remove-node g done))))

      )))



;914
