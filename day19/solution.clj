(ns day19.solution
  (:require [clojure.string :as str]))

(defn parse-workflow [workflow]
  (let [[_ name rest] (re-matches #"([a-z]+)\{(.+)\}" workflow)
        rules (reduce (fn [acc rule]
                        (let [[_ cat comp rating wf] (re-matches #"([xmas])([<>])(\d+):([a-zA-Z]+)" rule)
                              wf (if (nil? wf) rule wf)]
                          (conj acc {:cat cat
                                     :comp comp
                                     :rating (if rating (parse-long rating))
                                     :next-wf wf})))
                      []
                      (str/split rest #","))]
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
  "returns the next workflow name if the rule matches else nil"
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

(defn split-range [range val]
  (let [[from to] range]
    (cond
      (nil? from)  [[] []]
      (< val from) [[] range]
      (> val to)   [[] range]
      (= val to)   [range []]
      :else        [[from val] [(inc val) to]])))

(defn traverse-rules [workflows wf rules ratings res]
  "traverse rules tree and store results in 'res'. every rule has a true and false branch.
   the current category rating is split on every branch in a true range and a false range.
   the true branch follows the next workflow and the else branch follows the next rule.
   if workflow 'A' or 'R' is reached then the current branch is complete"
  (let [rule (first rules)
        {:keys [cat comp rating next-wf]} rule
        range (get ratings cat)
        ; split free ratings ranges in true and false branches based on the rule condition
        [left right] (case comp
                       ">" (let [[l r] (split-range range rating)] [r l])
                       "<" (split-range range (dec rating))
                       nil)
        new-ratings (if range
                      (assoc ratings cat left)
                      ratings)

        ; true branch (next workflow)
        left-res (case next-wf
                   "A" (assoc res :A (conj (:A res) new-ratings))
                   "R" (assoc res :R (conj (:R res) new-ratings))
                   (traverse-rules workflows
                                   next-wf
                                   (get workflows next-wf)
                                   new-ratings
                                   res))]
    ; else branch (next rule of same workflow)
    (if (> (count rules) 1)
      (traverse-rules workflows
                      wf
                      (rest rules)
                      (assoc new-ratings cat right)
                      left-res)
      left-res)))

(defn branch-combinations [ratings]
  (->> (vals ratings)
       (map (fn [[from to]]
              (inc (- to from))))
       (reduce *)))

(defn solve [input]
  (let [{:keys [workflows parts]} (parse input)
        accepted-parts (sort-parts workflows parts)]
    (->> (map vals accepted-parts)
         (flatten)
         (reduce + 0))))

(defn solve2 [input]
  (let [{:keys [workflows]} (parse input)
        branches (traverse-rules workflows
                                 "in"
                                 (get workflows "in")
                                 {"x" [1 4000] "m" [1 4000] "a" [1 4000] "s" [1 4000]}
                                 {:A [] :R []})]
    (->> (map branch-combinations (:A branches))
         (reduce +))))

(defn main []
  (let [text (slurp "day19/sample-input.txt")
        input (slurp "day19/input.txt")]

    ; sample
    (println "result - sample 1" (solve text))
    (println "result - sample 2" (solve2 text))

    ; solution
    (println "result - part 1" (solve input))
    (println "result - part 2" (solve2 input))
    ))