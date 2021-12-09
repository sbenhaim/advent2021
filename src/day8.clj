(ns day8
  (:require [clojure.string :as s]
            [clojure.set :refer [intersection]]))

(def input (s/split-lines (slurp "src/day8-input.txt")))

(defn parse-int [s]
  (Integer/parseInt s))

(def output
  (->> input
       (map #(s/split % #" \| "))
       (map second)
       (map #(s/split % #" "))))


;; Part 1

(->> output
     flatten
     (map count)
     (filter #{2 3 4 7}) ;; Here we're just counting the outputs with 2 3 4 and 7 segments
     count)

;; Part 2

(def signals
  (->> input
       (map #(s/split % #" "))
       (map #(map set %)) ;; Represent the digits with sets of their segments, for easy, unordered comparison
       (map #(vector (take 10 %) (drop 11 %))))) ;; Split the signal tests from the output digits

(defn count-of?
  "True if collection `s` has `n` items"
  [n s]
  (= n (count s)))

(defn count-intersects-of?
  "True if `n` items intersect between set `a` and `b`"
  [n a b]
  (= n (count (intersection a b))))

(defn find-only
  "Finds the unique item in the list that returns non-falsy for function `f`
  and returns it as a scalar. Throws unless exactly 1 result is found."
  [f s]
  (let [res (filter f s)]
    (assert (= 1 (count res)))
    (first res)))


(defn decode
  "Given the testing segments, determines all the represented digits
  and returns a map from the segment set to the digit"
  [input]
  (let [one   (find-only #(count-of? 2 %) input) ;; Only digit with 2 segments
        seven (find-only #(count-of? 3 %) input) ;; Only digit with 3 segments
        four  (find-only #(count-of? 4 %) input) ;; Only digit with 4 segments
        eight (find-only #(count-of? 7 %) input) ;; Only digit with 7 segments
        six   (find-only #(and (count-of? 6 %) ;; Only digit with 6 segments...
                               (count-intersects-of? 1 one %)) input) ;; and 1 intersecting with `one`
        nine  (find-only #(and (count-of? 6 %) ;; Only digit with 6 segments
                               (count-intersects-of? 4 four %)) input) ;; and 4 intersecting with `four`
        zero  (find-only #(and (count-of? 6 %) ;; Only digit with 6 segments
                               (not (#{six nine} %))) input) ;; that is also not six or nine ;)
        three (find-only #(and (count-of? 5 %) ;; Only digit with 5 segments
                               (count-intersects-of? 2 one %)) input) ;; and 2 intersecting with `one`
        two   (find-only #(and (count-of? 5 %) ;; Only digit with 5 segments
                               (count-intersects-of? 2 four %)) input) ;; and 2 intersecting with `four`
        five  (find-only #(and (= 5 (count %)) ;; Only digit with 5 segments
                               (count-intersects-of? 3 two %)) input)] ;; and 3 intersecting with `two`
    {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9}))

(defn parse
  "Given the segment->digit map produced by `decode`, decode the scrambled digits as an integer."
  [parse-map output]
  (->> output
       (map parse-map)
       s/join
       parse-int))

(->> signals
     (map (fn [[input output]] (parse (decode input) output)))
     (reduce +))
