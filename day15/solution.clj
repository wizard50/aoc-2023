(ns day15.solution
  (:require [clojure.string :as str])
  (:import (java.util.regex Pattern)))


(defn parse [input]
  (str/split (str/trim input) #","))

(defn parse-step [s]
  "s: (label)(operator)(focal length)"
  (let [operator (cond
                   (str/includes? s "=") "="
                   (str/includes? s "-") "-"
                   :else nil)
        [label focal-len] (str/split s (Pattern/compile operator))]
    {:label label
     :operator operator
     :focal-len (if focal-len (parse-long focal-len))}))

(defn hash-rfn [current-val ch]
  (-> current-val
      (+ (int ch))
      (* 17)
      (mod 256)))

(defn hash1 [s]
  (reduce hash-rfn 0 s))

(defn hash2 [step]
  "only using the label for hashing in this algo."
    (reduce hash-rfn 0 (:label step)))

(defn index-of-label [slots label]
  (first
    (for [i (range 0 (count slots))
          :let [step (nth slots i)]
          :when (= (:label step) label)]
      i)))

(defn list-update-at [list idx step]
  (vec (concat (take idx list) [step] (drop (inc idx) list))))

(defn list-delete-at [list idx]
  (vec (concat (take idx list) (drop (inc idx) list))))

(defn process-step [boxes step]
  (let [hash (hash2 step)
        slots (get boxes hash [])
        slot-idx (index-of-label slots (:label step))]
    (assoc boxes hash (case (:operator step)
                        "=" (if slot-idx
                              (list-update-at slots slot-idx step) ; PUT
                              (conj slots step))                   ; ADD
                        "-" (if slot-idx
                              (list-delete-at slots slot-idx)      ; DELETE
                              slots)))))

(defn focus-power [boxes]
  (reduce (fn [acc k]
            (let [slots (get boxes k)]
              (if (empty? slots)
                acc
                (reduce + acc (map #(* (inc k) (inc %) (:focal-len (get slots %)))
                                   (range 0 (count slots)))))))
          0
          (keys boxes)))

(defn solve [input]
  (->> (parse input)
       (map hash1)
       (reduce +)))

(defn solve2 [input]
  (->> (parse input)
       (map parse-step)
       (reduce process-step {})
       (focus-power)))

(defn main []
  (let [text (slurp "day15/sample-input.txt")
        input (slurp "day15/input.txt")]

    ; sample
    (println "sample 'HASH' value: " (reduce hash-rfn 0 "HASH"))
    (println "result - sample 1: " (solve text))
    (println "result - sample 2: " (solve2 text))

    ; solution
    (println "result - part 1: " (solve input))
    (println "result - part 2: " (solve2 input))

    ))

