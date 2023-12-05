(ns day03.solution
  (:require [clojure.string :as str]))

(def neighbour-pos [[-1 -1] [0 -1] [1 -1]                   ; prev line
                    [-1 0]  [1 0]                           ; cur line
                    [-1 1] [0 1] [1 1]])                    ; next line

(defn get-neighbours [x y context]
  (let [{:keys [x-dim y-dim lines]} context]
    (reduce (fn [neighbours p]
       (let [[xi yi] p
             xn (+ x xi)
             yn (+ y yi)]
         (if (and (>= xn 0) (< xn x-dim)
                  (>= yn 0) (< yn y-dim))
          (conj neighbours (nth (nth lines yn) xn))
          neighbours)
         )) [] neighbour-pos)
    ))

(defn collect-numbers [lines]
  (let [x-dim (count (first lines))
        y-dim (count lines)
        numbers (for [iy (range 0 y-dim)]
                  (loop [ix 0
                         numbers []
                         line (nth lines iy)]
                    (if (< ix x-dim)
                      (let [ch (nth line ix)
                            number-str (if (Character/isDigit ch)
                                         (str/join
                                           (take-while #(Character/isDigit %) (subs line ix))))
                            steps (if number-str (count number-str) 1)
                            neighbours (if number-str (->> (map #(get-neighbours % iy {:lines lines
                                                                                  :x-dim x-dim
                                                                                  :y-dim y-dim}) (range ix (+ ix steps)))
                                                           (reduce concat)
                                                           (distinct)))
                            adjacent? (if neighbours
                                        (reduce (fn [res n]
                                                  (if (not (str/includes? "0123456789." (str n))) true res))
                                                false neighbours))
                            next-ix (+ ix steps)
                            next-numbers (if adjacent?
                                           (conj numbers (Integer/parseInt number-str))
                                           numbers)]
                        (recur next-ix next-numbers line))
                      numbers)))
        numbers (reduce concat numbers)]
    numbers
  ))

(defn main []
  (let [text (slurp "day03/sample-input.txt")
        input (slurp "day03/input.txt")
        sample-numbers (collect-numbers (str/split-lines text))
        numbers1 (collect-numbers (str/split-lines input))]

    ; sample
    (println "engine schematic numbers - sample1: " sample-numbers)
    (println "sum sample1" (reduce + sample-numbers))

    ; solution
    (println "sum - part1: " (reduce + numbers1))))
