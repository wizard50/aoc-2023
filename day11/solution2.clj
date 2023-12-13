(ns day11.solution2
  (:require [clojure.string :as str]))

(defn find-galaxies [lines]
  "collect galaxy coordinates"
  (reduce (fn [acc [x y]]
            (if (= (nth (nth lines y) x) \#)
              (conj acc [x y])
              acc))
          []
          (for [y (range 0 (count lines))
                x (range 0 (count (first lines)))] [x y])))

(defn find-empty-rows [lines]
  (reduce (fn [acc y]
            (if (every? #(= % \.) (nth lines y))
              (conj acc y)
              acc))
          []
          (range 0 (count lines))))

(defn find-empty-cols [lines]
  (reduce (fn [acc x]
            (let [y-range (range 0 (count lines))]
              (if (every? #(= (nth (nth lines %) x) \.) y-range)
                (conj acc x)
                acc)))
          []
          (range 0 (count (first lines)))))

(defn transpose [[x y] expansion-gaps]
  (let [{:keys [rows cols n]} expansion-gaps
        nx (reduce (fn [acc gx]
                     (if (> x gx)
                       (+ acc (dec n))
                       acc))
                   x cols)
        ny (reduce (fn [acc gy]
                     (if (> y gy)
                       (+ acc (dec n))
                       acc))
                   y rows)]
    [nx ny]))

(defn calc-distance [[x1 y1] [x2 y2]]
  "calc horizontal + vertical steps to move"
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn calc-distances-between-galaxies [galaxies expansion-gaps]
  (reduce (fn [acc n]
            (let [g (nth galaxies n)
                  rest (nthrest galaxies (inc n))
                  distances (map #(calc-distance
                                    (transpose g expansion-gaps)
                                    (transpose % expansion-gaps)) rest)]
              (concat acc distances)))
          []
          (range 0 (dec (count galaxies)))))

(defn solve2 [input n]
  (let [lines (str/split-lines input)
        galaxies (find-galaxies lines)
        expansion-gaps {:rows (find-empty-rows lines)
                        :cols (find-empty-cols lines)
                        :n n}]
    (reduce + (calc-distances-between-galaxies galaxies expansion-gaps))
    ))

(defn main []
  (let [text (slurp "day11/sample-input.txt")
        input (slurp "day11/input.txt")]

    ; sample
    (println "sum of galaxy distances - sample 2, n=1" (solve2 text 2))
    (println "sum of galaxy distances - sample 2, n=10" (solve2 text 10))
    (println "sum of galaxy distances - sample 2, n=100" (solve2 text 100))

    ;; solution
    (println "sum of galaxy distances - part 1, n=2" (solve2 input 2))
    (println "sum of galaxy distances - part 2, n=1000000" (solve2 input 1000000))
    ))


