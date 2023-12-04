(ns day01.solution
  (:require [clojure.string :as str]))

(def words ["one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(defn filter-digits [line]
  "returns a list of digits"
  (filter #(Character/isDigit %) line))

(defn build-two-digit-number [digits]
  "combining the first digit and the last digit"
  (if (not (empty? digits))
    (-> (str (first digits) (last digits))
        (Integer/parseInt))
    0))

(defn word->digit [word]
  (let [index (.indexOf words word)]
    (if (not= index -1)
      (inc index))))

(defn starting-word [substr]
  "returns the starting digit word else nil"
  (reduce #(if (str/starts-with? substr %2) %2 %1)
          nil words))

(defn filter-digits2 [line]
  "filters digits and digit words. words can overlap so a simple replace does not give the correct answer."
  (reduce (fn [digits i]
            (let [ch (get line i)]
              (cond
                (Character/isDigit ch) (conj digits ch)
                (str/includes? "otfsen" (str ch)) (let [substr (subs line i)
                                                        word (starting-word substr)
                                                        digit (if word (word->digit word))]
                                                      (if digit
                                                        (conj digits digit)
                                                        digits))
                :else digits)))
          []
          (range 0 (count line))))

(defn line-values-part1 [text]
  "returns the two-digit numbers of each row - for solution part 1"
  (->> text
       (str/split-lines)
       (map filter-digits)
       (map build-two-digit-number)))

(defn line-values-part2 [text]
  "returns the two-digit numbers of each row - for solution part 2"
  (->> text
       (str/split-lines)
       (map filter-digits2)
       (map build-two-digit-number)))

(defn main []
  (let [text1 "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
        text2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
        calibration-values1 (line-values-part1 text1)
        calibration-values2 (line-values-part2 text2)
        input (slurp "src/day01/input.txt")]

    ; calibration
    (println "calibration values part1: " calibration-values1)
    (println "sum part1:                " (reduce + calibration-values1))

    (println "calibration values part2: " calibration-values2)
    (println "sum part2:                " (reduce + calibration-values2))

    ; solution
    (println "solution part1 - sum:" (reduce + (line-values-part1 input)))
    (println "solution part2 - sum:" (reduce + (line-values-part2 input)))
  ))
