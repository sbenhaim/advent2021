(ns day11
  (:require [clojure.string :as s]))

(def input "4575355623
3325578426
7885165576
4871455658
3722545312
8362663832
5562743324
4165776412
1817813675
4255524632")

(defn parse-int [s]
  (Integer/parseInt s))

(defn m-coords [m]
  (for [y (range (count m))
        x (range (count (first m)))]
    [y x]))

(defn make-matrix [input]
  (let [m (->> input
               (s/split-lines)
               (mapv #(s/split % #""))
               (mapv #(mapv parse-int %)))]
    m))


(defn inc-coords [[m flashes] [y x :as coords]]
  (let [v (get-in m coords)]
    (cond
      (nil? v) [m flashes]
      (= v 10) [m flashes]
      (< v 9) [(assoc-in m coords (inc v)) flashes]
      :else
      (reduce inc-coords
              [(assoc-in m coords 10) (inc flashes)]
              (for [i (range (dec x) (+ x 2))
                    j (range (dec y) (+ y 2))
                    :when (not= [j i] [y x])]
                [j i])))))

(defn step-matrix [[m flashes]]
  (let [[m flashes] (reduce inc-coords [m flashes] (m-coords m))]
    [(mapv #(replace {10 0} %) m) flashes]))


;; Part 1

(nth (iterate step-matrix [(make-matrix input) 0]) 100)

;; Part 2

(loop [[m flashes] [(make-matrix input) 0] step 0]
  (if (= 0 (apply + (flatten m)))
    step
    (recur (step-matrix [m flashes]) (inc step))))
