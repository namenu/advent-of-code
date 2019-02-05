(ns year2018.day15
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [graph :refer [bfs]]
            [util :refer [find-first manhattan-dist] :as util]))

(defn input->world [input]
  (let [cave (->> (str/split-lines input)
                  (map-indexed (fn [y line]
                                 (->> (map-indexed (fn [x c] [[y x] c]) line)
                                      (into {}))))
                  (apply merge))]
    {:walls (->> (filter (fn [[_ v]] (= \# v)) cave)
                 (map first)
                 (into #{}))
     :units (->> (filter (fn [[_ v]] (or (= \E v) (= \G v))) cave)
                 (mapv (fn [index [k v]]
                         {:id    index
                          :side  v
                          :pos   k
                          :hp    200
                          :power 3})
                       (range)))
     :round 0}))

(defn alive? [unit]
  (pos? (:hp unit)))

(defn enemy? [u1 u2]
  (not= (:side u1) (:side u2)))

(defn in-range? [u1 u2]
  (= 1 (manhattan-dist (:pos u1) (:pos u2))))

(defn nth-unit [world n]
  (get-in world [:units n]))

(defn alive-units [world]
  (filter alive? (:units world)))

(defn obstacles [world]
  (->> (alive-units world)
       (map :pos)
       (into (:walls world))))

(defn targets [world unit]
  (->> (alive-units world)
       (filter #(enemy? unit %))))

(defn attack-target [world unit]
  (->> (targets world unit)
       (filter #(in-range? unit %))
       (sort-by (juxt :hp :pos))
       (first)))

(defn neighbor* [world]
  (fn [pos]
    (->> [[-1 0] [1 0] [0 -1] [0 1]]
         (map #(mapv + pos %))
         (remove (obstacles world)))))

(defn destination [world unit]
  (let [dist-map  (bfs (-> unit :pos) (neighbor* world))
        reachable (->> (targets world unit)
                       (map :pos)
                       (mapcat (neighbor* world))
                       (distinct)
                       (filter dist-map))]
    (->> reachable
         (sort-by (juxt dist-map identity))
         (first))))

(defn next-pos [src dst adjacent]
  (let [move-map (bfs dst adjacent)
        adjacent (->> (adjacent src)
                      (filter move-map))]
    (->> adjacent
         (sort-by (juxt move-map identity))
         (first))))

(defn try-move [world index]
  (let [unit (nth-unit world index)]
    (if (attack-target world unit)
      world
      (if-let [dst (destination world unit)]
        (let [pos' (next-pos (:pos unit) dst (neighbor* world))]
          (assoc-in world [:units index :pos] pos'))
        world))))

(defn try-attack [world index]
  (let [unit (nth-unit world index)]
    (if-let [target (attack-target world unit)]
      (update-in world [:units (:id target) :hp] - (:power unit))
      world)))

(defn turn [world index]
  (let [unit (nth-unit world index)]
    (if (alive? unit)
      (-> world
          (try-move index)
          (try-attack index))
      world)))

(defn finished? [world]
  (let [sides (->> (alive-units world)
                   (group-by :side))]
    (= (count sides) 1)))

(defn round [world]
  (loop [world      world
         id-in-turn (->> (alive-units world)
                         (sort-by :pos)
                         (map :id))]
    (if-let [u (first id-in-turn)]
      (if (finished? world)
        world
        (recur (turn world u) (next id-in-turn)))
      (update world :round inc))))

(defn finish-combat [world]
  (->> (iterate round world)
       (find-first finished?)))

(defn outcome [world]
  (* (:round world)
     (->> (:units world)
          (map :hp)
          (filter pos?)
          (reduce +))))

(comment
  ; 27730, 4988
  (def input "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######")
  ; 36334, 29064
  (def input "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######")
  ; 39514, 31284
  (def input "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######")
  ; 27755, 3478
  (def input "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######")
  ; 28944, 6474
  (def input "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######")
  ; 18740, 1140
  (def input "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"))

(def input (-> "year2018/day15.in" io/resource slurp))

(defn part1 [input]
  (-> (input->world input)
      (finish-combat)
      (outcome)))

(defn adjust-power [world power]
  (let [power-fn (fn [unit]
                   (let [p (if (= (:side unit) \E) power 3)]
                     (assoc unit :power p)))]
    (update world :units #(mapv power-fn %))))

(defn no-loss? [world side]
  (->> (:units world)
       (filter #(= (:side %) side))
       (every? alive?)))

(defn part2 [input]
  (let [world (input->world input)]
    (->> (drop 4 (range))
         (map #(adjust-power world %))
         (map finish-combat)
         (find-first #(no-loss? % \E))
         (outcome))))

(defn print-world [{:keys [walls round] :as world}]
  (let [[_ [max-y max-x]] (util/bounding-box (:walls world))
        units    (alive-units world)
        yx->unit (reduce #(assoc %1 (:pos %2) %2) {} units)]
    (println "After" round "rounds:")
    (doseq [y (util/range-incl max-y)]
      (doseq [x (util/range-incl max-x)]
        (print (if (walls [y x])
                 \#
                 (or (get-in yx->unit [[y x] :side]) \.))))
      (doseq [u (->> units
                     (filter #(= (first (:pos %)) y))
                     (sort-by :pos))]
        (print "" (str (:side u) "(" (:hp u) ")")))
      (println))))
