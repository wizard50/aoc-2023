(ns day10.solution
  (:require [clojure.string :as str]))

(def neighbour-pos [[0 -1] [-1 0] [1 0] [0 1]])             ; N W E S

(def pipes {\F [[1 0] [0 1]]
            \| [[0 -1] [0 1]]
            \L [[1 0] [0 -1]]
            \- [[-1 0] [1 0]]
            \J [[-1 0] [0 -1]]
            \7 [[-1 0] [0 1]]})

(defn get-neighbours [[x y] lines]
  (let [x-dim (count (first lines))
        y-dim (count lines)]
    (reduce (fn [neighbours rel-pos]
              (let [[xi yi] rel-pos
                    xn (+ x xi)
                    yn (+ y yi)]
                (if (and (>= xn 0) (< xn x-dim)
                         (>= yn 0) (< yn y-dim))
                  (conj neighbours {:tile (nth (nth lines yn) xn)
                                    :pos [xn yn]})
                  neighbours)))
            []
            neighbour-pos)))

(defn find-start-pos [lines]
  (loop [y 0 pos nil]
    (if (and (< y (count lines)) (not pos))
      (recur (inc y)
             (let [line (nth lines y)
                   x (.indexOf line "S")]
               (if (>= x 0) [x y])))
      pos)))

(defn calc-pos [[x y] [x-rel y-rel]]
  [(+ x x-rel) (+ y y-rel)])

(defn equal-pos? [[x1 y1] [x2 y2]]
  (and (= x1 x2) (= y1 y2)))

(defn reachable-pos? [pos tile nb]
  (let [pipes (get pipes tile)
        nb-coords (map #(calc-pos pos %) pipes)]
    (not (empty? (filter #(equal-pos? (:pos nb) %) nb-coords)))))

(defn filter-tiles [neighbours tiles]
  (filter (fn [nb]
            (some #(= % (:tile nb)) tiles))
          neighbours))

(defn get-pipe-nbs [lines pos]
  (filter-tiles (get-neighbours pos lines) (keys pipes)))

(defn move [lines cur-pos & [prev-pos]]
  (let [[x y] cur-pos
        tile (nth (nth lines y) x)
        pipes-nb (get-pipe-nbs lines cur-pos)
        next-nb (if prev-pos
                  (->> pipes-nb
                       (filter #(not (equal-pos? prev-pos (:pos %))))
                       (filter #(reachable-pos? cur-pos tile %)))
                  pipes-nb)]
    (if (not (empty? next-nb))
      (:pos (first next-nb)))))

(defn get-pipe-loop [lines & [start-pos first-nb-pos]]
  (loop [cur-pos (if first-nb-pos first-nb-pos (find-start-pos lines))
         prev-pos (if start-pos start-pos)
         path []]
    (if cur-pos
      (let [next-pos (move lines cur-pos prev-pos)]
        (recur next-pos
               cur-pos
               (conj path cur-pos)))
      path)))

(defn solve [input]
  (let [lines (str/split-lines input)
        start-pos (find-start-pos lines)
        pipe-nbs (get-pipe-nbs lines start-pos)
        pipe-loops (map #(get-pipe-loop lines start-pos (:pos %))
                       pipe-nbs)
        max-dist-list (map (fn [pipe-loop]
                            (int (Math/ceil (/ (count pipe-loop) 2))))
                          pipe-loops)]
    (reduce max max-dist-list)))

(defn main []
  (let [text (slurp "day10/sample-input.txt")
        text2 (slurp "day10/sample-input2.txt")
        input (slurp "day10/input.txt")]

    ; sample
    (println "result - sample 1: " (solve text))
    (println "result - sample 1 (complex): " (solve text2))

    ; solution
    (println "result - part 1: " (solve input))
    ))

