(ns year2018.day24
  (:require [util :refer [rsort-by fixed-point find-first]]
            [clojure.string :as str]))

(def input
  {:immune-system [{:units 17 :hp 5390 :immune #{} :weak #{:radiation :bludgeoning} :attack 4507 :type :fire :initiative 2 :group 1}
                   {:units 989 :hp 1274 :immune #{:fire} :weak #{:bludgeoning :slashing} :attack 25 :type :slashing :initiative 3 :group 2}]
   :infection     [{:units 801 :hp 4706 :immune #{} :weak #{:radiation} :attack 116 :type :bludgeoning :initiative 1 :group 1}
                   {:units 4485 :hp 2961 :immune #{:radiation} :weak #{:fire :cold} :attack 12 :type :slashing :initiative 4 :group 2}]})

(def input
  {:immune-system [{:units 3400, :hp 1430, :attack 4, :initiative 4, :type :radiation, :immune #{:fire :radiation :slashing}, :weak #{}}
                   {:units 138, :hp 8650, :attack 576, :initiative 16, :type :slashing, :immune #{:radiation :slashing :cold}, :weak #{:bludgeoning}}
                   {:units 255, :hp 9469, :attack 351, :initiative 8, :type :bludgeoning, :immune #{}, :weak #{:fire :radiation}}
                   {:units 4145, :hp 2591, :attack 6, :initiative 12, :type :fire, :immune #{:cold}, :weak #{:slashing}}
                   {:units 3605, :hp 10989, :attack 26, :initiative 19, :type :fire, :immune #{}, :weak #{}}
                   {:units 865, :hp 11201, :attack 102, :initiative 10, :type :slashing, :immune #{}, :weak #{}}
                   {:units 633, :hp 10092, :attack 150, :initiative 11, :type :slashing, :immune #{}, :weak #{:radiation :slashing}}
                   {:units 2347, :hp 3322, :attack 12, :initiative 2, :type :cold, :immune #{}, :weak #{}}
                   {:units 7045, :hp 3877, :attack 5, :initiative 5, :type :bludgeoning, :immune #{}, :weak #{:radiation}}
                   {:units 1086, :hp 8626, :attack 69, :initiative 13, :type :slashing, :immune #{}, :weak #{:radiation}}]
   :infection     [{:units 2152, :hp 12657, :attack 11, :initiative 18, :type :fire, :immune #{}, :weak #{:fire :cold}}
                   {:units 40, :hp 39458, :attack 1519, :initiative 7, :type :slashing, :immune #{:fire :radiation :slashing}, :weak #{:bludgeoning}}
                   {:units 59, :hp 35138, :attack 1105, :initiative 15, :type :fire, :immune #{:radiation}, :weak #{:fire}}
                   {:units 1569, :hp 51364, :attack 55, :initiative 17, :type :radiation, :immune #{}, :weak #{:radiation}}
                   {:units 929, :hp 23887, :attack 48, :initiative 14, :type :cold, :immune #{}, :weak #{:bludgeoning}}
                   {:units 5264, :hp 14842, :attack 4, :initiative 9, :type :bludgeoning, :immune #{:fire :cold}, :weak #{:bludgeoning :slashing}}
                   {:units 1570, :hp 30419, :attack 35, :initiative 1, :type :slashing, :immune #{:fire}, :weak #{:radiation :cold}}
                   {:units 1428, :hp 21393, :attack 29, :initiative 6, :type :cold, :immune #{}, :weak #{:radiation}}
                   {:units 1014, :hp 25717, :attack 47, :initiative 3, :type :fire, :immune #{}, :weak #{:fire}}
                   {:units 7933, :hp 29900, :attack 5, :initiative 20, :type :slashing, :immune #{:bludgeoning :radiation :slashing}, :weak #{}}]})

(defn parse-line [s]
  (let [group  (->> (re-seq #"\d+" s)
                    (map #(Integer/parseInt %))
                    (map vector [:units :hp :attack :initiative])
                    (into {}))
        type   (-> (re-find #"(\w+) damage" s) second keyword)

        immune (->> (some-> (re-find #"immune to (.*?)[\);]" s) second (str/split #", "))
                    (map keyword))
        weak   (->> (some-> (re-find #"weak to (.*?)[\);]" s) second (str/split #", "))
                    (map keyword))]

    (-> group
        (assoc :type type)
        (assoc :immune (set immune))
        (assoc :weak (set weak)))))

(defn input->groups [input]
  (reduce-kv (fn [res army groups]
               (into res (map #(assoc % :army army) groups)))
             []
             input))

(defn effective-power [group]
  (* (:units group) (:attack group)))

(defn selection-order [groups]
  (rsort-by (juxt effective-power :initiative) groups))

(defn damage [atk def]
  (let [deal (effective-power atk)
        coef (cond
               ((:immune def) (:type atk)) 0
               ((:weak def) (:type atk)) 2
               :else 1)]
    (* deal coef)))

(defn enemy-army [groups group]
  (filter #(not= (:army %) (:army group)) groups))

(defn selection
  "Returns a map of attacker->defender"
  [groups]
  (reduce
    (fn [selection attacker]
      (let [chosen (set (vals selection))
            target (->> (enemy-army groups attacker)
                        (filter (comp pos? :units))
                        (remove chosen)
                        (rsort-by (juxt #(damage attacker %) effective-power :initiative))
                        (first))]
        (if (and target (pos? (damage attacker target)))
          (assoc selection attacker target)
          selection)))
    {}
    (selection-order groups)))

(defn deal-damage [group damage]
  (let [loss (quot damage (:hp group))
        left (max (- (:units group) loss) 0)]
    (assoc group :units left)))

(defn index-of [coll item]
  (loop [i 0 s coll]
    (when s
      (if (= (first s) item)
        i
        (recur (inc i) (next s))))))

(defn round [groups]
  (let [selection (selection groups)
        order     (->> (rsort-by (comp :initiative first) selection)
                       (map (fn [[atk def]] [(index-of groups atk) (index-of groups def)])))]
    ; in decreasing initiative order
    (reduce
      (fn [groups [atk-i def-i]]
        (let [atk   (nth groups atk-i)
              def   (nth groups def-i)
              dealt (damage atk def)
              def'  (deal-damage def dealt)]
          (assoc groups def-i def')))
      groups
      order)))

(defn units-by-army [groups]
  (->> (group-by :army groups)
       (reduce-kv (fn [m k v]
                    (assoc m k (reduce + (map :units v))))
                  {})))

;part-1
(let [groups (input->groups input)]
  (units-by-army (fixed-point round groups)))

(defn boost-power [groups boost]
  (mapv #(update % :attack + (if (= (:army %) :immune-system) boost 0)) groups))

(let [groups (input->groups input)]
  (->> (range)
       (map #(boost-power groups %))
       (map #(units-by-army (fixed-point round %)))
       (find-first #(zero? (:infection %)))))