(ns year2023.graph.mincut
  (:require [year2023.graph :as g]))

(defn random-edge [g]
  ;[u v] (rand-nth (edges g))
  (let [u (rand-nth (g/vertices g))
        v (rand-nth (seq (g/vertices-from g u)))]
    [u v]))

(defn contract [g t]
  (loop [g    g
         vcnt (count (g/vertices g))]
    (if (> vcnt t)
      (let [e (random-edge g)]
        (recur (g/contract-edge g e)
               (dec vcnt)))
      g)))

(defn fast-mincut [g count-cuts]
  (let [vcnt (count (g/vertices g))]
    (if (<= vcnt 6)
      (let [st   (contract g 2)
            cuts (count-cuts (g/groups st))]
        ;; find answers immediately
        (when (<= cuts 4)
          (prn cuts)
          (when (= cuts 3)
            (prn st)))
        [st cuts])
      (let [t    (int (Math/ceil (+ 1 (/ vcnt (Math/sqrt 2)))))
            g1   (contract g t)
            g2   (contract g t)
            cut1 (fast-mincut g1 count-cuts)
            cut2 (fast-mincut g2 count-cuts)]
        (min-key second cut1 cut2)))))

(defn karger [g]
  (let [count-cuts (let [es (set (g/edges g))]
                     (fn [groups]
                       (assert (= (count groups) 2))
                       (let [[s t] groups]
                         (/ (count (remove (fn [[u v]]
                                             (or (and (s u) (s v))
                                                 (and (t u) (t v)))) es))
                            2))))]
    (fast-mincut g count-cuts)))
