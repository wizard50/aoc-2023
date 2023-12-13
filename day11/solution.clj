(ns day11.solution
  (:require [clojure.string :as str]))

(defn expand-rows [lines]
  "add an additional empty row after each empty row"
  (reduce (fn [acc line]
            (if (every? #(= % \.) line)
              (conj acc line line)
              (conj acc line)))
          []
          lines))

(defn expand-cols [lines]
"add an additional col next to each empty col"
(reduce (fn [acc x]
          (let [y-range (range 0 (count acc))]
            (if (every? #(= (nth (nth lines %) x) \.) y-range)
              (map #(str/join [% \. \.]) acc)
              (map #(str/join [(nth acc %)
                               (nth (nth lines %) x)])
                   y-range))))
        (repeat (count lines) "")
        (range 0 (count (first lines)))))

(defn find-galaxies [lines]
  "collect galaxy coordinates"
  (reduce (fn [acc [x y]]
            (if (= (nth (nth lines y) x) \#)
              (conj acc [x y])
              acc))
          []
          (for [y (range 0 (count lines))
                x (range 0 (count (first lines)))] [x y])))

(defn calc-distance [[x1 y1] [x2 y2]]
  "calc horizontal + vertical steps to move"
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn calc-distances-between-galaxies [galaxies]
  (reduce (fn [acc n]
            (let [g (nth galaxies n)
                  rest (nthrest galaxies (inc n))
                  distances (map #(calc-distance g %) rest)]
              (concat acc distances)))
          []
          (range 0 (dec (count galaxies)))))

(defn solve1 [input]
  (let [universe (expand-rows
                   (expand-cols (str/split-lines input)))
        galaxies (find-galaxies universe)]
    (reduce + (calc-distances-between-galaxies galaxies))
    ))

(defn main []
  (let [text (slurp "day11/sample-input.txt")
        input (slurp "day11/input.txt")]

    ; sample
    (println "sum of galaxy distances" (solve1 text))

    ; solution
    (println "sum of galaxy distances" (solve1 input))
    ))

