(ns year2023.day5
  (:require [clojure.string :as str]))

(defn parse-seeds [s]
  (->> (str/split s #" ")
       (drop 1)
       (map parse-long)))

(defn parse-converter [lines]
  (->> (str/split-lines lines)
       (drop 1)
       (map (fn [s]
              (->> (str/split s #" ")
                   (map parse-long))))))

(defn parse-input [input]
  (let [[seeds & converters] (str/split input #"\n\n")]
    {:seeds      (parse-seeds seeds)
     :converters (map parse-converter converters)}))

(defn convert [converter x]
  (if-let [[dst src] (first (filter (fn [[_ src r]]
                                      (<= src x (dec (+ src r))))
                                    converter))]
    (+ dst (- x src))
    x))

(defn part1 [input]
  (let [data    (parse-input input)
        process (fn [seed]
                  (reduce (fn [v converter]
                            (convert converter v))
                          seed
                          (:converters data)))]
    (->> (:seeds data)
         (map process)
         (apply min))))

(defn preprocess [{:keys [seeds converters]}]
  {:seeds      (->> (partition 2 seeds)
                    (map (fn [[l len]]
                           [l (+ l len)])))
   :converters (map (fn [converter]
                      (map (fn [[dst src len]]
                             [src (+ src len) (- dst src)])
                           converter))
                    converters)})

(defn get-segments [converter [l r]]
  (let [xs       (->> converter
                      (mapcat (fn [[l r _]] [l r]))
                      (sort)
                      (dedupe)
                      (drop-while #(<= % l))
                      (take-while #(< % r)))
        segments (->> (flatten [l xs r])
                      (partition 2 1))
        convert  (fn [[l r]]
                   (if-let [[_ _ dc] (first (filter (fn [[lc rc _]]
                                                      (and (< l rc) (< lc r)))
                                                    converter))]
                     [(+ l dc) (+ r dc)]
                     [l r]))]
    (map convert segments)))

(defn solve [[c & converters] in-segs]
  (if c
    (let [out-segs (mapcat #(get-segments c %) in-segs)]
      (solve converters out-segs))
    in-segs))

(comment
  (def input-sample "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")
  (def input-large (slurp "resources/day5.in"))

  ;; part2
  (let [{:keys [seeds converters]} (preprocess (parse-input input-large))]
    (ffirst (sort (solve converters seeds)))))
