;; --- Day 25: Cryostasis ---
(ns aoc.year2019.day25
  (:refer-clojure :exclude [take drop])
  (:require [aoc.util :refer [input]]
            [aoc.year2019.intcode :refer :all]
            [clojure.math.combinatorics :as combo]))

(defn run-cmd [droid cmd & [opts]]
  ;(println ">> " cmd)
  (let [opts (merge {:ascii true} opts)
        feed (map int cmd)
        [droid' output] (-> (reduce #(add-input %1 %2) droid feed)
                            (run)
                            (get-output opts))]
    ;(println output)
    droid'))

(defn move [droid cmds]
  (let [cmds (apply str (map {\S "south\n"
                              \E "east\n"
                              \N "north\n"
                              \W "west\n"} cmds))]
    (run-cmd droid cmds)))

(defn take [droid target]
  (let [cmd (str "take " target "\n")]
    (run-cmd droid cmd)))

(defn drop [droid obj]
  (let [cmd (str "drop " obj "\n")]
    (run-cmd droid cmd)))

(defn inv [droid]
  (run-cmd droid "inv"))

(def items
  ;;; do not touch: infinite loop, photons, escape pod, molten lava, giant electromagnet(?)
  {"asterisk"     "SW"
   "boulder"      "S"
   "candy cane"   "E"
   "food ration"  "SE"
   "prime number" "ENNN"
   "loom"         "EEN"
   "mug"          "ENEN"
   "mutex"        "ENN"})

(defn take-and-back [droid target]
  (let [moves     (items target)
        backmoves (map {\S \N \N \S \E \W \W \E} (reverse moves))]
    (-> droid (move moves) (take target) (move backmoves))))

(defn try-combination [droid items]
  (let [[droid' output] (-> (reduce take-and-back droid items)
                            (move "EEESEE")
                            (run-cmd "north\n" {:keep true})
                            (get-output {:ascii true}))]
    (println items (re-find #"\".+\"", output))
    droid'))

(defn combinations [items]
  (->> (range 1 (inc (count items)))
       (mapcat #(combo/combinations items %))))

;pt.1
(let [droid (input->machine (input 2019 25))]
  ; no need to branch and bound
  #_(dorun (->> (combinations (keys items))
                (map #(try-combination droid %))))
  (-> droid
      (try-combination ["asterisk" "prime number" "mug" "mutex"])
      (get-output {:ascii true}))
  'OK)


; E, bed: [candy cane]
; EN, Stables: [infinite loop]
; ENN, Hallway: [mutex]
; ENNW, Arcade: [giant electromagnet]
; ENNN, Hot choco fountain: [prime number]
; ENNNE, Corridor : [escape pod]
; ENE, Engineering
; ENEN, Navigation: [mug]

; EE, Kitchen
; EEN, Storage: [loom]
; EEE, Warp Drive Maint
; EEES, Passage
; EEESE, Sickbay: [photons]
; EEESEE, security check point
; EEESEEN, Pressure-sensitive Floor
; EEESW, Gift Wrapping Center [molten lava]

; S, Holodeck: [boulder]
; SE, Sciencelab: [food ration]
; SW, Observatory, [asterisk]
