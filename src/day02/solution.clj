(ns day02.solution
  (:require [clojure.string :as str]))

(defn parse-cube [text]
  (as-> (str/split text #" ") x
        (map #(str/trim %) x)
        (filter #(not (empty? %)) x)
        (let [[v k] x]
          {(keyword k) (Integer/parseInt v)})))

(defn parse-cube-set [text]
  (->> (map parse-cube (str/split text #","))
       (reduce conj {})))

(defn parse-line [line]
  (let [[game sets] (str/split line #":")]
    {:game (-> (second (str/split game #" "))
               (Integer/parseInt))
     :sets (map parse-cube-set (str/split sets #";"))
     }))

(defn get-game-list [input]
  (->> input
       (str/split-lines)
       (map parse-line)))

(defn get-max [cube-sets key]
  "filters the sets by key and returns the max value"
  (->> (map #(get % key 0) cube-sets)
       (reduce max 0)))

(defn game-possible? [game limit-set]
  (let [cube-sets (:sets game)]
    (and (>= (:red limit-set) (get-max cube-sets :red))
         (>= (:blue limit-set) (get-max cube-sets :blue))
         (>= (:green limit-set) (get-max cube-sets :green)))))

(defn calc-sum [games limit-set]
  (let [possible (map #(game-possible? % limit-set) games)
        game-ids (map #(:game %) games)]
    (reduce #(let [possible? (nth possible %2)
                   game-id  (nth game-ids %2)]
               (if possible? (+ %1 game-id) %1))
            0 (range 0 (count games)))))

(defn calc-power [games]
  (map #(* (get-max (:sets %) :red)
           (get-max (:sets %) :blue)
           (get-max (:sets %) :green)) games))

(defn main []
  (let [text (slurp "src/day02/sample-input1.txt")
        input (slurp "src/day02/input.txt")
        limit-set {:red 12 :green 13 :blue 14}
        sample-games (get-game-list text)
        games (get-game-list input)]

    ; sample1
    (println "games possible - sample1: " (map #(game-possible? % limit-set) sample-games))
    (println "game id sum - sample1:    " (calc-sum sample-games limit-set))
    (println "power - sample2           " (calc-power sample-games))
    (println "power sum - sample2       " (reduce + (calc-power sample-games)))

    ; solution
    (println "solution part1 - sum:" (calc-sum games limit-set))
    (println "solution part2 - power sum:" (reduce + (calc-power games)))
    ))
