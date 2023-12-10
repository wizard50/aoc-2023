(ns day08.solution
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [[inst nodes] (str/split input #"\n\n")
        node-lines (str/split-lines nodes)]
    {:inst (str/trim inst)
     :nodes (reduce #(let [[name left right] (re-seq  #"\w+" %2)]
                            (assoc %1 name {:name name :left left :right right}))
                    {}
                    node-lines)}))

(defn navigate [map]
  (let [len (count (:inst map))]
    (loop [i 0
           node (get (:nodes map) "AAA")
           finish? false]
      (if (not finish?)
        (let [inst (nth (:inst map) (mod i len))
              next-name (get node (case inst
                                    \L :left
                                    \R :right))]
          (recur (inc i)
                 (get (:nodes map) next-name)
                 (= next-name "ZZZ")))
        i))))

(defn solve [input]
  (navigate (parse input)))

(defn main []
  (let [text (slurp "day08/sample-input.txt")
        text2 (slurp "day08/sample-input2.txt")
        input (slurp "day08/input.txt")]

    ; sample
    (println "navigation steps needed (RL) - sample 1:" (solve text))
    (println "navigation steps needed (LLR) - sample 1:" (solve text2))

    ; solution
    (println "navigation steps needed - part 1:" (solve input))
    ))

