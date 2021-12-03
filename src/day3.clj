(ns day3
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/day3-input.txt")))

;; Part 1

(defn most-common [v]
  (if (>= (apply + v)
         (/ (count v) 2))
    1 0))

(defn least-common [v]
  (if (>= (apply + v)
         (/ (count v) 2))
    0 1))

(defn vnot [v]
  (mapv {1 0 0 1} v))

(defn bvec->int [bvec]
  (Integer/parseInt (s/join bvec) 2))

(defn mult-with-inverse [v]
  (let [inv (vnot v)]
    (* (bvec->int v) (bvec->int inv))))

(println
 (->> input
      (mapv vec) ;; List of chars
      (mapv #(mapv {\0 0 \1 1} %)) ;; Convert to ints
      (apply mapv list) ;; Transpose
      (mapv most-common) ;; List of most common
      (mult-with-inverse))) ;; Multiply by binary inverse

;; Part 2

(defn filter-down [f vs]
  (loop [remaining vs n 0]
    (let [v (map #(nth % n) remaining)
          mc (f v)
          mask (mapv #{mc} v)
          remaining (keep-indexed (fn [idx v] (and (nth mask idx) v)) remaining)]
      (if (= 1 (count remaining))
        (first remaining)
        (recur remaining (inc n))))))

(defn process [input f]
  (->> input
       (mapv vec) ;; List of chars
       (mapv #(mapv {\0 0 \1 1} %))
       (filter-down f)))

(println
 (let [mc (process input most-common)
       lc (process input least-common)]
   (* (bvec->int mc) (bvec->int lc))))
