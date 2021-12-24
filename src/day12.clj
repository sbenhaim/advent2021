(ns day12
  (:require [clojure.string :as s]
            [clojure.set :refer [union]]))

(def input "yb-start
de-vd
rj-yb
rj-VP
OC-de
MU-de
end-DN
vd-end
WK-vd
rj-de
DN-vd
start-VP
DN-yb
vd-MU
DN-rj
de-VP
yb-OC
start-rj
oa-MU
yb-de
oa-VP
jv-MU
yb-MU
end-OC")


(defn type-node [n]
  (cond
    (= n "start") [:small :start]
    (= n "end") :end
    (re-matches #"[A-Z]+" n) [:big n]
    :else [:small n]))

(defn build-map [input]
  (let [pairs (->> input
                   (s/split-lines)
                   (map #(s/split % #"-")))]
    (loop [m {} [[from to] & pairs] pairs]
      (let [from-node (type-node from)
            to-node (type-node to)
            m (update-in m [from-node] union #{to-node})
            m (update-in m [to-node] union #{from-node})]
        (if (seq pairs)
          (recur m pairs)
          m)))))

(defn find-path
  ([m] (find-path m [[:small :start]]))
  ([m route]
   (let [node (last route)]
     (if (= :end node)
       route
       (let [inelig (set (filter (fn [[size _]] (= size :small)) route))
             next-nodes (remove inelig (m node))]
         (if (seq next-nodes)
           (mapcat #(find-path m (conj route %)) next-nodes)
           []))))))

(defn count-paths [input]
  (count
   (filter #{[:small :start]}
           (find-path (build-map input)))))

;; Part 1

(count-paths input)

;; Part 2

(defn ineligible [route]
  (let [small-caves (filter (fn [[size _]] (= size :small)) route)
        fs (vals (frequencies small-caves))]
    (if (every? #{1} fs)
      #{[:small :start]}
      (set small-caves))))

(defn find-path2
  ([m] (find-path2 m [[:small :start]]))
  ([m route]
   (let [node (last route)]
     (if (= :end node)
       route
       (let [inelig (ineligible route)
             next-nodes (remove inelig (m node))]
         (if (seq next-nodes)
           (mapcat #(find-path2 m (conj route %)) next-nodes)
           []))))))

(defn count-paths2 [input]
  (count
   (filter #{[:small :start]}
           (find-path2 (build-map input)))))

(count-paths2 input)
