(ns year2018.day17
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (str/split-lines "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"))
(def input (->> (-> "day17.in" io/resource io/reader line-seq)))

(defn parse-line [s]
  (let [[_ axis v _ begin end] (re-find #"(\w)=(\d+), (\w)=(\d+)\.\.(\d+)" s)
        v (Integer/parseInt v)
        r (range (Integer/parseInt begin) (inc (Integer/parseInt end)))]
    (if (= axis "x")
      (map vector (repeat v) r)
      (map vector r (repeat v)))))

(defn debug-print [water]
  (let [min-x (apply min (map first clay))
        max-x (apply max (map first clay))]
    (doseq [y (range 0 (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (print
          (cond
            (clay [x y]) \#
            (water [x y]) (water [x y])
            :else \space)))
      (println))))

(def clay (->> (mapcat parse-line input)
               (into #{})))

(def min-y (apply min (map second clay)))
(def max-y (apply max (map second clay)))

(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])
(defn down [[x y]] [x (inc y)])

(defn flow [water [_ y :as pos]]
  (let [water     (assoc water pos \|)

        sand?     (fn [pos]
                    (not (or (clay pos) (water pos))))

        overflow? (fn [pos]
                    (or (clay pos)
                        (= (water pos) \~)))

        spill?    (fn [pos]
                    (let [test (fn [pos move]
                                 (if (clay pos)
                                   false
                                   (if (overflow? (down pos))
                                     (recur (move pos) move)
                                     true)))]
                      (or (test (left pos) left)
                          (test (right pos) right))))

        spill     (fn [water pos move]
                    (if (and (sand? (move pos))
                             (overflow? (down pos)))
                      (flow water (move pos))
                      water))

        fill      (fn [water pos move]
                    (if (clay pos)
                      water
                      (recur (assoc water pos \~) (move pos) move)))
        ]

    (cond

      ; should stop?
      (>= y max-y)
      water

      ; can flow-down?
      (sand? (down pos))
      ; flow-down & try overflow
      (-> water
          (flow (down pos))
          (recur pos))

      ; can spill left or right?
      (spill? pos)
      (-> water
          (spill pos left)
          (spill pos right))

      ; is blocked both sides?
      :else
      (-> water
          (assoc pos \~)
          (fill (left pos) left)
          (fill (right pos) right)))
    ))

(def water (flow {} [500 min-y]))

;part1
(count water)

;part2
(- (count water)
   (count (filter #(= (val %) \|) water)))

(comment
  (spit "out.txt" (with-out-str (debug-print water))))