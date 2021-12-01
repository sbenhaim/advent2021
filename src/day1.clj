(ns day1
  (:require [clojure.string :as s]))

;; Part 1
(let [input (slurp "src/day1-input.txt")]
  (->> input
       (s/split-lines)
       (map #(Integer/parseInt %))
       (partition 2 1) ;; Sliding 2-item window
       (map (fn [[a b]] (if (> b a) 1 0))) ;; Is item 1 > item 0?
       (reduce +))) ;; Sum

;; Part 2
(let [input (slurp "src/day1-input.txt")]
  (->> input
       (s/split-lines)
       (map #(Integer/parseInt %))
       (partition 3 1) ;; Sliding 3-item window
       (partition 2 1) ;; Pair 3-item windows
       (map (fn [[a b]] (if (> (apply + b) (apply + a)) 1 0))) ;; Sum of window 1 > sum of window 0
       (reduce +))) ;; Sum
