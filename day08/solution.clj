(ns day08.solution
  (:require [clojure.string :as str]))

(defn gcd
  ([x y]
   (cond (zero? x) y
         (< y x)   (recur y x)
         :else     (recur x (rem y x)))))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

(defn parse [input]
  (let [[inst nodes] (str/split input #"\n\n")]
    {:inst (str/trim inst)
     :nodes (reduce #(let [[name left right] (re-seq  #"\w+" %2)]
                            (assoc %1 name {:left left :right right}))
                    {}
                    (str/split-lines nodes))}))

(defn get-node-keys [map ends-with]
  (filter #(str/ends-with? % ends-with) (keys (:nodes map))))

(defn navigate [map & [start-key end-fn]]
  (let [len (count (:inst map))
        start-key (if start-key start-key "AAA")
        end-fn (if end-fn end-fn #(= % "ZZZ"))]
    (loop [i 0
           node (get (:nodes map) start-key)
           finish? false]
      (if (not finish?)
        (let [inst (nth (:inst map) (mod i len))
              next-key (get node (case inst
                                    \L :left
                                    \R :right))]
          (recur (inc i)
                 (get (:nodes map) next-key)
                 (end-fn next-key)))
        i))))

(defn solve [input]
  (navigate (parse input)))

(defn solve2 [input]
  (let [data (parse input)
        steps (map (fn [k]
                    (navigate data k #(str/ends-with? % "Z")))
                  (get-node-keys data "A"))]
  (reduce lcm steps)))

(defn main []
  (let [text (slurp "day08/sample-input.txt")
        text2 (slurp "day08/sample-input2.txt")
        input (slurp "day08/input.txt")]

    ; sample
    (println "navigation steps needed (RL) - sample 1:" (solve text))
    (println "navigation steps needed (LLR) - sample 1:" (solve text2))

    (println "starting node keys" (get-node-keys (parse input) "A"))
    (println "ending node keys" (get-node-keys (parse input) "Z"))

    ;; solution
    (println "navigation steps needed - part 1:" (solve input))
    (println "navigation steps needed - part 2:" (solve2 input))))
