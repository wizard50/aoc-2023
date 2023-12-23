(ns day18.solution
  (:require [clojure.string :as str]))

(defn get-point [[x y] dir n]
  (case dir
    "R" [(+ x n) y]
    "L" [(- x n) y]
    "D" [x (+ y n)]
    "U" [x (- y n)]))

(defn parse-line1 [line]
  (let [[dir n _] (str/split line #" ")]
    {:dir dir
     :n (parse-long n)}))

(defn parse-line2 [line]
  (let [[_ _ rgb] (str/split line #" ")
        rgb (second (re-matches #"\(#([a-fA-F\d]{6})\)" rgb))]
    {:dir (case (subs rgb 5 6)
           "0" "R"
           "1" "D"
           "2" "L"
           "3" "U")
     :n (read-string (str "0x" (subs rgb 0 5)))}))

(defn parse-dig-plan [input parse-line-fn]
  (let [lines (str/split-lines input)]
    (loop [i 0
           point [0 0]
           items []]
      (if (< i (count lines))
        (let [line (nth lines i)
              {:keys [dir n]} (parse-line-fn line)
              next-point (get-point point dir n)
              item {:dir dir
                    :n n
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

(defn solve [input parse-line-fn]
  (let [dig-plan (parse-dig-plan input parse-line-fn)
        points (map #(:point %) dig-plan)
        area (shoelace points)
        boundary (reduce + 0 (map :n dig-plan))
        interior (interior area boundary)]
    (+ interior boundary)))

(defn main []
  (let [text (slurp "day18/sample-input.txt")
        input (slurp "day18/input.txt")]

    ; sample
    (println "result - sample 1:" (solve text parse-line1))
    (println "result - sample 2:" (solve text parse-line2))

    ; solution
    (println "result - part 1" (solve input parse-line1))
    (println "result - part 2" (solve input parse-line2))
    ))

