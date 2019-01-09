(ns year2018.day15
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [graph :refer [bfs]]
            [util :refer [manhattan-dist]]))

(defn input->world [input]
  (let [cave (->> (str/split-lines input)
                  (map-indexed (fn [y line]
                                 (into {} (map-indexed (fn [x c] [[y x] c]) line))))
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

(defn obstacle? [{:keys [walls units]} pos]
  (or (walls pos)
      (let [obstacles (->> (filter alive? units)
                           (map :pos)
                           (into #{}))]
        (obstacles pos))))

(defn neighbor* [world]
  (fn [pos]
    (->> [[-1 0] [1 0] [0 -1] [0 1]]
         (map #(mapv + pos %))
         (remove #(obstacle? world %)))))

(defn targets [{:keys [units]} unit]
  (->> (filter alive? units)
       (filter #(enemy? unit %))))

(defn attack-target [world unit]
  (->> (targets world unit)
       (filter #(in-range? unit %))
       (sort-by (juxt :hp :pos))
       (first)))

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

(defn to-move [world unit dst]
  (let [move-map (bfs dst (neighbor* world))
        adjacent (->> ((neighbor* world) (:pos unit))
                      (filter move-map))]
    (->> adjacent
         (sort-by (juxt move-map identity))
         (first))))

(defn approach [world unit]
  (if (attack-target world unit)
    world
    (if-let [dst (destination world unit)]
      (let [pos' (to-move world unit dst)]
        (assoc-in world [:units (:id unit) :pos] pos'))
      world)))

(defn try-attack [world index]
  (let [unit (get-in world [:units index])]
    (if-let [target (attack-target world unit)]
      (update-in world [:units (:id target) :hp] - (:power unit))
      world)))

(defn turn [world index]
  (let [unit (get-in world [:units index])]
    (if (alive? unit)
      (-> world
          (approach unit)
          (try-attack (:id unit)))
      world)))

(defn finished? [world]
  (let [sides (->> (:units world)
                   (filter alive?)
                   (group-by :side))]
    (= (count sides) 1)))

(defn round [world]
  (loop [world        world
         units-to-act (->> (filter alive? (:units world))
                           (sort-by :pos)
                           (map :id))]
    (if-let [u (first units-to-act)]
      (if (finished? world)
        world
        (recur (turn world u) (next units-to-act)))
      (update world :round inc))))

(defn finish-combat [world]
  (->> (iterate round world)
       (drop-while (complement finished?))
       (first)))

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

(def input (-> "day15.in" io/resource slurp))

; part-1
(-> (input->world input)
    (finish-combat)
    (outcome))

; part-2
(defn adjust-power [world power]
  (let [power-fn (fn [unit]
                   (let [p (if (= (:side unit) \E) power 3)]
                     (assoc unit :power p)))]
    (update world :units #(mapv power-fn %))))

(let [world (input->world input)
      loss? #(and (= (:side %) \E) ((complement alive?) %))]
  (->> (drop 4 (range))
       (map #(adjust-power world %))
       (map finish-combat)
       (drop-while #(some loss? (:units %)))
       (first)
       (outcome)))

(defn print-world [{:keys [walls units round]}]
  (let [max-y    (inc (apply max (map first walls)))
        max-x    (inc (apply max (map second walls)))

        units    (filter alive? units)
        yx->unit (reduce #(assoc %1 (:pos %2) %2) {} units)]
    (println "After" round "rounds:")
    (doseq [y (range max-y)]
      (doseq [x (range max-x)]
        (print (if (walls [y x])
                 \#
                 (or (get-in yx->unit [[y x] :side]) \.))))
      (doseq [u (->> units
                     (filter #(= (first (:pos %)) y))
                     (sort-by :pos))]
        (print "" (str (:side u) "(" (:hp u) ")")))
      (println))))
