(ns day19.solution
  (:require [clojure.string :as str]))

(defn parse-workflow [workflow]
  (let [[_ name r] (re-matches #"([a-z]+)\{(.+)\}" workflow)
        rules (reduce (fn [acc rule]
                        (let [[_ cat comp rating wf] (re-matches #"([xmas])([<>])(\d+):([a-zA-Z]+)" rule)
                              wf (if (nil? wf) rule wf)]
                          (conj acc {:cat cat
                                     :comp comp
                                     :rating (if rating (parse-long rating))
                                     :next-wf wf})))
                      []
                      (str/split r #","))]
  {:name name :rules rules}))

(defn parse-part [part]
  (reduce (fn [acc pair]
            (let [[k v] (str/split pair #"=")]
              (assoc acc k (parse-long v))))
          {}
          (re-seq #"[xmas]=\d+" part)))

(defn parse [input]
  (let [[w p] (str/split input #"\n\n")
        workflows (reduce #(let [{:keys [name rules]} (parse-workflow %2)]
                             (assoc %1 name rules))
                          {}
                          (str/split-lines w))
        parts (map parse-part (str/split-lines p))]
    {:workflows workflows :parts parts}))

(defn eval-rule [rule part]
  (let [{:keys [cat comp rating next-wf]} rule
        part-val (get part cat)
        rule-matches? (case comp
                        nil true ; last rules have only a workflow name
                        ">" (> part-val rating)
                        "<" (< part-val rating)
                        false)]
    (if rule-matches? next-wf)))

(defn accept-part? [part workflows wf]
  "this is a recursive method which iterates all rules of the current workflow
   until the next workflow is determined.
   each part will be accepted or rejected by one of the workflow rules,
   so the termination is given when the current workflow is A or B"
  (case wf
    "A" true
    "R" false
    (let [rules (get workflows wf)]
      (loop [i 0
             next-wf nil]
        (if (and (< i (count rules)) (not next-wf))
          (recur (inc i)
                 (eval-rule (nth rules i) part))
          (accept-part? part workflows next-wf))))))

(defn sort-parts [workflows parts]
  (reduce (fn [acc part]
            ; accepted parts have a result, rejected nil
            (let [res (accept-part? part workflows "in")]
              (if res
                (conj acc part)
                acc)))
          []
          parts))

(defn solve [input]
  (let [{:keys [workflows parts]} (parse input)
        accepted-parts (sort-parts workflows parts)]
    (->> (map vals accepted-parts)
         (flatten)
         (reduce + 0))))

(defn main []
  (let [text (slurp "day19/sample-input.txt")
        input (slurp "day19/input.txt")]

    ; sample
    (println "result - sample 1" (solve text))

    ; solution
    (println "result - part 1" (solve input))
    ))

