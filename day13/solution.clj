(ns day13.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map #(str/split-lines %))))

(defn get-mirrored-cols [lines x]
  (for [y (range 0 (count lines))
        :let [line (nth lines y)
              len (min x (- (count line) x))
              start (if (<= x len)
                      0
                      (- (count line) (* len 2)))
              col1 (subs line start (+ start len))
              col2 (subs line (+ start len) (+ start (* 2 len)))] ]
    [col1 (str/reverse col2)]))

(defn search-v-reflection [lines]
  (for [x (range 1 (count (first lines)))
        :when (every? #(= (first %) (second %))
                      (get-mirrored-cols lines x))]
    x))

(defn search-h-reflection [lines]
  (for [y (range 1 (count lines))
        :let [len (min y (- (count lines) y))
              start (if (<= y len)
                      0
                      (- (count lines) (* len 2)))
              rows1 (str/join (reverse (take len (nthnext lines start))))
              rows2 (str/join (take len (nthnext lines (+ start len))))]
        :when (= rows1 rows2)]
    y))

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn list-update-in [list idx replacement]
  (concat (take idx list) [replacement] (drop (inc idx) list)))

(defn swap-cell [cell]
  (if (= cell \#) \. \#))

(defn search-smudge [lines]
  (for [y (range 0 (count lines))
        x (range 0 (count (first lines)))
        :let [line (nth lines y)
              cell (nth line x)
              newline (replace-at line x (swap-cell cell))
              newpattern (list-update-in lines y newline)
              v-reflection (search-v-reflection newpattern)
              h-reflection (search-h-reflection newpattern)]
        :when (or (not (empty? v-reflection))
                  (not (empty? h-reflection)))]
    {:v v-reflection :h h-reflection}))

(defn solve [input]
  (let [pattern-list (parse input)]
    (reduce (fn [acc pattern]
              (let [h (first (search-h-reflection pattern))
                    v (first (search-v-reflection pattern))]
                (+ acc (if v v 0)
                       (if h (* h 100) 0))))
            0
            pattern-list)))

(defn solve2 [input]
  (let [pattern-list (parse input)]
    (reduce (fn [acc pattern]
            (let [smg (search-smudge pattern)
                  smg-v (reduce #(concat  %1 (:v %2)) [] smg)
                  smg-h (reduce #(concat %1 (:h %2)) [] smg)
                  old-v (search-v-reflection pattern)
                  old-h (search-h-reflection pattern)
                  v-diff (set/difference (into #{} smg-v) (into #{} old-v))
                  h-diff (set/difference (into #{} smg-h) (into #{} old-h))]
              (+ acc (if (first v-diff) (first v-diff) 0)
                 (if (first h-diff) (* (first h-diff) 100) 0))))
            0
            pattern-list)))

(defn main []
  (let [text (slurp "day13/sample-input.txt")
        input (slurp "day13/input.txt")]

    ; sample
    (println "result - sample 1: " (solve text))
    (println "result - sample 2: " (solve2 text))

    ; solution
    (println "result - part 1: " (solve input))
    (println "result - part 2: " (solve2 input))
    ))

