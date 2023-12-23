(ns day18.solution
  (:require [clojure.string :as str]))

(defn get-point [[x y] dir n]
  (case dir
    "R" [(+ x n) y]
    "L" [(- x n) y]
    "D" [x (+ y n)]
    "U" [x (- y n)]))

(defn parse-dig-plan [input]
  (let [lines (str/split-lines input)]
    (loop [i 0
           point [0 0]
           items []]
      (if (< i (count lines))
        (let [line (nth lines i)
              [dir n rgb] (str/split line #" ")
              n (parse-long n)
              next-point (get-point point dir n)
              item {:dir dir
                    :n n
                    :rgb (-> rgb
                             (str/replace #"\(#" "")
                             (str/replace #"\)" ""))
                    :point next-point}]
          (recur
            (inc i)
            next-point
            (conj items item)))
        items))))

(defn shoelace [points]
  "https://en.wikipedia.org/wiki/Shoelace_formula"
  (let [points (conj (vec points) (first points))
        a (for [i (range 1 (count points))
                :let [[x0 y0] (nth points (dec i))
                      [x1 y1] (nth points i)]]
              (* (+ y0 y1) (- x0 x1)))]
    (abs (/ (reduce + 0 a) 2))))

(defn interior [area boundary]
  "https://en.wikipedia.org/wiki/Pick%27s_theorem
  A = interior + boundary / 2 - 1
  interior = A - boundary / 2 + 1"
  (+ (- area (/ boundary 2)) 1))

(defn solve [input]
  (let [dig-plan (parse-dig-plan input)
        points (map #(:point %) dig-plan)
        area (shoelace points)
        boundary (reduce + 0 (map :n dig-plan))
        interior (interior area boundary)]
    (+ interior boundary)))

(defn main []
  (let [text (slurp "day18/sample-input.txt")
        input (slurp "day18/input.txt")]

    ; sample
    (println "result - sample 1:" (solve text))

    ; solution
    (println "result - part 1" (solve input))
    ))

