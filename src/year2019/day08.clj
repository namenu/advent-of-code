;; --- Day 8: Sunny with a Chance of Asteroids ---
(ns year2019.day08
  (:require [util :refer [input find-first]]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

; layer-size wide, tall
(def wide 25)
(def tall 6)

(defn merge-layers [l1 l2]
  (map (fn [a b]
         (if (= a \2)
           b
           a))
       l1 l2))

(defn render-layer [layer]
  (doseq [line (partition wide layer)]
    (println (apply str (map {\0 "◾️" \1 "▫️️️️"} line)))))


(let [layers (->> (input 2019 8)
                  (partition (* wide tall)))]
  ; pt.1
  (let [min0-layer (->> layers
                        (map frequencies)
                        (apply min-key #(% \0)))]
    (* (min0-layer \1) (min0-layer \2)))

  ; pt.2
  (render-layer (reduce merge-layers layers))
  )
