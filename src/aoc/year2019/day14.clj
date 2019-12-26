;; --- Day 14: Space Stoichiometry ---
(ns aoc.year2019.day14
  (:require [aoc.util :refer [input]]
            [clojure.string :as str]))

(defn parse-reaction [line]
  (let [[l r] (str/split line #" => ")
        parse #(let [[q m] (str/split % #" ")]
                 [(symbol m) (Integer/parseInt q)])
        [m q] (parse r)
        ]
    [m {:quantity q
        :req      (->> (str/split l #", ")
                       (map parse)
                       (into {}))}]))

(defn parse-input [in]
  (->> (str/split-lines in)
       (map parse-reaction)
       (into {})))

(defn topology [from edges visited sorted]
  (let [vs (->> (filter #(= (first %) from) edges)
                (map second)
                (remove visited))]
    (if (empty? vs)
      [visited (conj sorted from)]
      (loop [[v & vs] vs
             [visited sorted] [visited sorted]]
        (if v
          (recur vs (topology v edges (conj visited v) sorted))
          [visited (conj sorted from)])))))

(defn ceil-div [n d]
  (+ (quot n d)
     (if (zero? (rem n d)) 0 1)))


(defn update-vals
  "Applies f to all the vals in the map"
  [m f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} (or m {})))

(defn r-produce [reactions required to-produce]
  (let [q-req    (required to-produce)
        reaction (reactions to-produce)]
    (if q-req
      (let [multiplier (ceil-div q-req (:quantity reaction))]
        (merge-with + (dissoc required to-produce) (update-vals (:req reaction) #(* % multiplier))))
      required)))

(defn fuel->ore [reactions amount]
  (let [edges (->> (mapcat (fn [[v {:keys [req]}]]
                             (map #(vector % v) (keys req))) reactions)
                   (into #{}))
        order (-> (topology 'ORE edges #{'ORE} [])
                  (second))]
    ('ORE (reduce (partial r-produce reactions)
                  {'FUEL amount}
                  (butlast order)))))

(defn binary-search
  "[l, h)"
  [pred l h]
  (loop [l l h h]
    (let [m (quot (+ h l) 2)]
      (if (< l m)
        (if (pred m)
          (recur m h)
          (recur l m))
        l))))

(let [in        "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"
      in        "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"
      in        "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT\n"
      in        "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF\n"
      in        "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX"
      in        (input 2019 14)
      reactions (parse-input in)]

  ; pt.1
  (fuel->ore reactions 1)

  ; pt.2
  (let [trillion 1000000000000]
    (binary-search #(<= (fuel->ore reactions %) trillion) 0 10000000))
  )
