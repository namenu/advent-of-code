(ns year2023.day25
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [year2023.graph :as g]
            [year2023.graph.mincut :refer [karger]]))

(defrecord G [es]
  g/UndirectedUnweightedGraph
  (vertices [_]
    (keys es))
  (edges [_]
    (mapcat (fn [[u ws]]
              (map #(vector u %) ws))
            es))
  (add-vertex [_ v ws]
    (let [;; add edges from v
          es (update es v (fnil set/union #{}) (set ws))
          ;; add edges to v
          es (reduce (fn [e w]
                       (update e w (fnil conj #{}) v))
                     es
                     ws)]
      (->G es)))
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
  (vertices-from [_ v]
    (es v))
  (contract-edge [this [u v]]
    (let [uv (str u ":" v)
          ws (set/union (disj (g/vertices-from this u) v) (disj (g/vertices-from this v) u))]
      (-> this
          (g/remove-vertex u)
          (g/remove-vertex v)
          (g/add-vertex uv ws))))
  (groups [this]
    (->> (g/vertices this)
         (map #(set (str/split % #":"))))))

(def input-sample "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr")
(def input-large (slurp "resources/day25.in"))

(defn parse-input [input]
  (let [parse-line (fn [line]
                     (let [[u vs] (str/split line #": ")]
                       [u (str/split vs #" ")]))]
    (->> (str/split-lines input)
         (map parse-line)
         (reduce (fn [g [u ws]]
                   (g/add-vertex g u ws))
                 (->G {})))))

(comment
  ;; part 1
  (let [g0 (parse-input input-large)
        [st cuts] (karger g0)]
    (let [[s t] (g/groups st)]
      (* (count s) (count t))))


  ;; convert to .dot format
  (println
    (-> (str/replace input-sample ": " "--")
        (str/replace " " ","))))