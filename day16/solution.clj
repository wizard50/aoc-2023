(ns day16.solution
  (:require [clojure.string :as str]))

(def tile-pos {\. {:right [[1 0]]        :left [[-1 0]]        :up [[0 -1]]       :down [[0 1]]}
               \/ {:right [[0 -1]]       :left [[0 1]]         :up [[1 0]]        :down  [[-1 0]]}
               \\ {:right [[0 1]]        :left [[0 -1]]        :up [[-1 0]]       :down [ [1 0]]}
               \- {:right [[1 0]]        :left [[-1 0]]        :up [[-1 0] [1 0]] :down [[-1 0] [1 0]]}
               \| {:right [[0 -1] [0 1]] :left [[0 -1] [0 1]]  :up [[0 -1]]       :down [[0 1]]}})

(defn parse [input]
  (str/split-lines input))

(defn inside-grid? [[x y] x-dim y-dim]
  (and (>= x 0) (< x x-dim)
       (>= y 0) (< y y-dim)))

(defn known-move? [pos-map pos dir]
  (and (not (nil? pos))
       (contains? pos-map pos)
       (some #(= dir %) (get pos-map pos))))

(defn get-tile [grid [x y]]
  (nth (nth grid y) x))

(defn rel-pos->dir [[x y]]
  (cond
    (= x -1) :left
    (= x 1)  :right
    (= y -1) :up
    (= y 1)  :down))

(defn translate-pos [[x y] [x-rel y-rel]]
  [(+ x x-rel) (+ y y-rel)])

(defn valid? [grid energized step]
  (let [x-dim (count (first grid))
        y-dim (count grid)
        [pos dir] step]
    (and (not (nil? pos))
         (inside-grid? pos x-dim y-dim)
         (not (known-move? energized pos dir)))))

(defn get-next-steps [pos dir tile]
  (let [rel-pos (if (= nil? pos)
                  []
                  (get-in tile-pos [tile dir]))
        pos (map #(translate-pos pos %) rel-pos)
        dir (map #(rel-pos->dir %) rel-pos)]
    (map vector pos dir)))

(defn move [grid steps energized]
  (let [[pos dir] (first steps)
        tile (get-tile grid pos)
        new-steps (filter #(valid? grid energized %)
                           (get-next-steps pos dir tile))
        steps (concat (drop 1 steps) new-steps)
        energized (assoc energized pos (conj (if (get energized pos)
                                               (get energized pos)
                                               [])
                                             dir))]
    (if (not (empty? steps))
      (recur grid
             steps
             energized)
      energized)))

(defn solve [input]
  (->> (move (parse input)
             [[[0 0] :right]]
             {})
       (count)))

(defn main []
  (let [text (slurp "day16/sample-input.txt")
        input (slurp "day16/input.txt")]

    ; sample
    (println "result - sample 1" (solve text))

    ; solution
    (println "result - part 1" (solve input))
    ))