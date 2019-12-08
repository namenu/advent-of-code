;; --- Day 5: Sunny with a Chance of Asteroids ---
(ns year2019.day05
  (:require [util :refer [input find-first]]
            [clojure.string :as str]
            [year2019.intcode :refer :all]))

(defn input->program [input]
  (let [numbers (-> input str/trim (str/split #","))]
    (mapv #(Integer/parseInt %) numbers)))

(defn diagnostic-code [program input]
  (let [state (-> (->machine program) (add-input input) (run*))
        diag  (find-first (complement zero?) (:output state))]
    (or diag 0)))


(let [program (input->program (input 2019 5))]
  ; pt.1
  (diagnostic-code program 1)

  ; pt.2
  (diagnostic-code program 5))
