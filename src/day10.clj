(ns day10
  (:require [clojure.string :as s]
            [clojure.set :refer [map-invert]]))

(def test ["[({(<(())[]>[[{[]{<()<>>"
           "[(()[<>])]({[<{<<[]>>("
           "{([(<{}[<>[]}>{[]{[(<()>"
           "(((({<>}<{<{<>}{[]{[]{}"
           "[[<[([]))<([[{}[[()]]]"
           "[{[{({}]{}}([{[{{{}}([]"
           "{<[[]]>}<{[{[{[]{()[[[]"
           "[<(<(<(<{}))><([]([]()"
           "<{([([[(<>()){}]>(<<{{"
           "<{([{{}}[<[[[<>{}]]]>[]]"])

(def input (s/split-lines (slurp "src/day10-input.txt")))


(def pair
  {\} \{
   \) \(
   \] \[
   \> \<})


(def score
  {\} 1197
   \) 3
   \] 57
   \> 25137})

(def close? (set (keys pair)))

(def open? (set (vals pair)))

(defn validate [input]
  (loop [[char & remaining] input stack []]
    (if (nil? char)
      (if (seq stack) [:incomplete] [:valid])
      (if (open? char)
        (recur remaining (conj stack char))
        (if (= (pair char) (last stack))
          (recur remaining (vec (butlast stack)))
          [:corrupt char])))))

(->> input
     (map validate)
     (keep (fn [[outcome chr]] (when (= :corrupt outcome) (score chr))))
     (apply +))

;; Part 2

(def unpair (map-invert pair))

(defn validate2 [input]
  (loop [[char & remaining] input stack []]
    (if (nil? char)
      (if (seq stack) [:incomplete (map unpair (reverse stack))] [:valid])
      (if (open? char)
        (recur remaining (conj stack char))
        (if (= (pair char) (last stack))
          (recur remaining (vec (butlast stack)))
          [:corrupt char])))))

(def score2
  {\} 3
   \) 1
   \] 2
   \> 4})

(defn calc-score [completion]
  (reduce (fn [score char] (+ (score2 char) (* 5 score))) 0 completion))

(let [scores (->> input
                  (map validate2)
                  (keep (fn [[outcome completion]] (when (= :incomplete outcome) (calc-score completion))))
                  sort)
      half (quot (count scores) 2)
      mid (first (drop half scores))]
  mid)
