(ns year2023.graph)


(defprotocol UndirectedUnweightedGraph
  (vertices [_])
  (edges [_])
  (add-vertex [_ v ws])
  (remove-vertex [_ v])
  (vertices-from [_ v])

  (contract-edge [_ e])
  (groups [_]))
