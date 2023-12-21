(ns day17.solution
  (:require [clojure.string :as str]
              [clojure.data.priority-map :refer [priority-map]]))

(def next-dirs {:right [:up :down]
                :left   [:up :down]
                :up     [:left :right]
                :down   [:left :right]})

(defn next-pos [[x y] dir n]
  "get next pos by moving in the given direction"
  (case dir
    :right [(+ x n) y]
    :left  [(- x n) y]
    :up    [x (- y n)]
    :down  [x (+ y n)]))

(defn parse [input]
  (str/split-lines input))

(defn parse-graph [grid]
  "graph is a map with pos as key: [x y] => weight"
  (reduce (fn [acc [x y]]
            (assoc acc [x y] (parse-long (str (nth (nth grid y) x)))))
          {}
          (for [y (range 0 (count grid))
                x (range 0 (count (first grid)))]
            [x y])))

(defn valid? [visited graph nbs]
  (filter (fn [[p d n]]
            (let [cur-pos (next-pos p d n)]
              (and (contains? graph cur-pos)
                   (not (contains? visited [cur-pos d])))))
          nbs))

(defn get-neighbours [pos dir min-move max-move]
  "a neighbour is defined by the tuple: (origin pos, new direction, steps count)"
  (let [nb-dirs (get next-dirs dir)]
    (for [d (range 0 (count nb-dirs))
          i (range min-move (inc max-move))]
      [pos (nth nb-dirs d) i])))

(defn get-dist [graph [p d n] dist-parent]
  (reduce #(let [cur-pos (next-pos p d %2)
                 weight (get graph cur-pos)]
             (+ %1 weight))
          dist-parent
          (range 1 (inc n))))

(defn traverse-graph [graph start-pos end-pos min-steps max-steps]
  "dijkstra based algorithm. constraint: min and max steps in one direction then a turn is needed"
  (loop [queue (priority-map [start-pos :right] 0
                             [start-pos :down] 0)           ; key: [pos dir], value: priority
         visited #{}
         shortest-distance nil]
    (if (and (not (empty? queue))
             (not (= (first (first (peek queue))) end-pos)))
      (let [[[pos dir] dist] (peek queue)
            visited (if (not (contains? visited [pos dir]))
                      (conj visited [pos dir] {})
                      visited)
            nbs (->> (get-neighbours pos dir min-steps max-steps)
                     (valid? visited graph))
            next-queue (reduce (fn [acc [p d n]]
                                 (let [cur-pos (next-pos p d n)
                                       cur-dist (get-dist graph [p d n] dist)
                                       q-dist (get queue [cur-pos d])]
                                   ; only put new entries or entries with a shorter distance into the map,
                                   (if (or (not q-dist) (< cur-dist q-dist))
                                     (assoc acc [cur-pos d] cur-dist)
                                     acc)))
                               (pop queue)
                               nbs)]
        (recur next-queue
               visited
               (let [[[pos dir] dist] (peek next-queue)]
                 (if (= pos end-pos) dist))))
      shortest-distance)))

(defn solve [input min-steps max-steps]
  (let [grid (parse input)
        graph (parse-graph grid)
        x-max (dec (count (first grid)))
        y-max (dec (count grid))]
    (traverse-graph graph [0 0] [x-max y-max] min-steps max-steps)))

(defn main []
  (let [text (slurp "day17/sample-input.txt")
        text2 (slurp "day17/sample-input2.txt")
        input (slurp "day17/input.txt")]

    ; sample
    (println "result - sample 1:" (solve text 1 3))
    (println "result - sample 2.1:" (solve text 4 10))
    (println "result - sample 2.2:" (solve text2 4 10))

    ; solution
    (println "result - part 1:" (solve input 1 3))
    (println "result - part 2:" (solve input 4 10))
    ))