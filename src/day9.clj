(ns day9
  (:require [clojure.string :as s]
            [clojure.set :refer [difference union]]))

(def input (slurp "src/day9-input.txt"))


(defn parse-int [s]
  (Integer/parseInt s))

(def matrix
  (->> input
       (s/split-lines)
       (mapv #(s/split % #""))
       (mapv #(mapv parse-int %))))

(def m
  [[2 1 9 9 9 4 3 2 1 0]
   [3 9 8 7 8 9 4 9 2 1]
   [9 8 5 6 7 8 9 8 9 2]
   [8 7 6 7 8 9 6 7 8 9]
   [9 8 9 9 9 6 5 6 7 8]])

(defn adj-coords [[y x]]
  (set [[(dec y) x]
        [y (dec x)]
        [(inc y) x]
        [y (inc x)]]))

(defn adj [m coords]
  (keep identity
        (mapv #(get-in m %) (adj-coords coords))))

(defn low-point? [m coords]
  (< (get-in m coords) (apply min (adj m coords))))

;; Part 1

(def low-points
  (let [h (count matrix)
        w (count (first matrix))]
    (for [y (range h)
          x (range w)
          :when (low-point? matrix [y x])]
      [y x])))

(reduce + (map #(inc (get-in matrix %)) low-points))


;; Part 2

(defn basin
  ([m coords] (basin m coords #{}))
  ([m coords plumbed]
   (let [depth (get-in m coords)
         plumbed (conj plumbed coords)]
     (if (or (nil? depth) (= 9 depth)) [#{} plumbed]
         (let [adj (adj-coords coords)
               remaining (difference adj plumbed)]
           (reduce (fn [[prev-basin plumbed] coords]
                     (let [[new-basin plumbed] (basin m coords plumbed)]
                       [(union prev-basin new-basin) plumbed]))
                   [#{coords} plumbed]
                   remaining))))))


(->> low-points
     (map #(count (first (basin matrix %))))
     sort
     reverse
     (take 3)
     (reduce *))
