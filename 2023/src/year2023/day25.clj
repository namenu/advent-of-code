(ns year2023.day25
  (:require [clojure.string :as str]
            [clojure.set :as set]))


(defprotocol UndirectedUnweightedGraph
  (remove-vertex [_ v])
  (add-vertex [_ v ws])
  (vertices-from [_ v])
  (edges [_])
  (vertices [_])
  (contract-edge [_ e]))

(defprotocol KargerMinCut
  (-contract [_ t])
  (-fast-mincut [_]))

;; all vertices are connected at least one edge
(defrecord G [es]
  UndirectedUnweightedGraph
  (remove-vertex [_ v]
    (let [ws (es v)
          ;; remove edges from v
          es (dissoc es v)
          ;; remove edges to v
          es (reduce (fn [e w]
                       (update e w disj v))
                     es
                     ws)]
      (->G es)))
  (add-vertex [_ v ws]
    (let [;; add edges from v
          es (update es v (fnil set/union #{}) (set ws))
          ;; add edges to v
          es (reduce (fn [e w]
                       (update e w (fnil conj #{}) v))
                     es
                     ws)]
      (->G es)))
  (vertices-from [_ v]
    (es v))
  (edges [_]
    (mapcat (fn [[u ws]]
              (map #(vector u %) ws))
            es))
  (vertices [this]
    (reduce (fn [vs [u v]]
              (conj vs u v))
            #{}
            (edges this)))

  (contract-edge [this [u v]]
    (let [uv (str u ":" v)
          ws (set/union (disj (vertices-from this u) v) (disj (vertices-from this v) u))]
      (-> this
          (remove-vertex u)
          (remove-vertex v)
          (add-vertex uv ws)))))

(def input-sample "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr")
(def input-large (slurp "resources/day25.in"))

(defn parse-input [input]
  (let [parse-line (fn [line]
                     (let [[u vs] (str/split line #": ")]
                       [u (str/split vs #" ")]))]
    (->> (str/split-lines input)
         (map parse-line)
         (reduce (fn [g [u ws]]
                   (add-vertex g u ws))
                 (->G {})))))

(let [g (parse-input input-sample)]
  (-> g
      (contract-edge ["hfx" "bvb"])))

(defn contract [g t]
  (if (> (count (vertices g)) t)
    (let [[u v] (rand-nth (edges g))]
      (recur (contract-edge g [u v]) t))
    g))


(defn cuttable [g find-cuts]
  (reify
    KargerMinCut
    (-contract [_ t]
      (loop [g g]
        (if (> (count (vertices g)) t)
          (let [[u v] (rand-nth (edges g))]
            (recur (contract-edge g [u v])))
          g)))

    (-fast-mincut [_]
      (if (<= (count (vertices g)) 6)
        (do
          (prn (count (vertices g)))
          (contract g 2))
        (let [t  (int (Math/ceil (+ 1 (/ (count (vertices g)) (Math/sqrt 2)))))
              g1 (contract g t)
              g2 (contract g t)
              k1 (-fast-mincut (cuttable g1 find-cuts))
              k2 (-fast-mincut (cuttable g2 find-cuts))]
          (min-key #(count (find-cuts %)) k1 k2))))))

(let [g0        (parse-input input-large)
      es        (set (edges g0))
      find-cuts (fn [g]
                  ;; g must be a bipartitioned graph
                  (let [[s t] (vec (vertices g))]
                    (set/intersection (set (for [u (str/split s #":")
                                                 v (str/split t #":")]
                                             [u v]))
                                      es)))]
  #_(-contract (cuttable g0 count-cuts) 2)
  (find-cuts (-fast-mincut (cuttable g0 find-cuts))))