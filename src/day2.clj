(ns day2
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/day2-input.txt")))

(defn calc-outcome [move-fn init]
  (let [res (->> input
                 (map #(s/split % #" "))
                 (map (fn [[c l]] [c (Integer/parseInt l)]))
                 (reduce move-fn (merge {:pos 0 :depth 0} init)))]
    (* (:pos res) (:depth res))))

;; Quesiton 1

(defn move [acc [c l]]
  (case c
    "forward" (update-in acc [:pos] + l)
    "down" (update-in acc [:depth] + l)
    "up" (update-in acc [:depth] - l)))

(println (calc-outcome move {}))

;; Question 2

(defn move2 [acc [c l]]
  (case c
    "forward" (-> acc
                  (update-in [:pos] + l)
                  (update-in [:depth] + (* l (:aim acc))))
    "down" (update-in acc [:aim] + l)
    "up" (update-in acc [:aim] - l)))

(println (calc-outcome move2 {:aim 0}))
