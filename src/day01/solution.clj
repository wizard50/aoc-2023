(ns day01.solution)

(defn filter-digits [line]
  (filter #(Character/isDigit %) line))

(defn part1 []
  "Day 1 solution part 1"
  (let [calibration-values (->> (slurp "src/day01/input.txt")
                                (clojure.string/split-lines)
                                (map filter-digits)
                                (map #(str (first %) (last %)))
                                (map #(Integer/parseInt %)))]
    (println "calibration values: " calibration-values)
    (println "sum:                " (reduce + calibration-values))
    ))