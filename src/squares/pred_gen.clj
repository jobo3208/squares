(ns squares.pred-gen
  "Code for generating predicates dynamically"
  (:require [clojure.math.combinatorics :as combo]
            [clojure.set :as se]
            [squares.core :as core]))

(def max-attempts 50)
(def pred-pool-size 20)
(def max-coincidence-rate 1/2)
(def max-answer-rate 1/4)

(defmulti generate-pred
  "Multimethod to generate a predicate of the given class, optionally
  using the given sample data."
  (fn [backend pred-class sample]
    pred-class))

(defn- coincidence-rate
  ([sample-pred-results pred-pair]
   (let [[x y] pred-pair]
     (max (coincidence-rate sample-pred-results x y)
          (coincidence-rate sample-pred-results y x))))
  ([sample-pred-results x y]
   (let [px (count (filter #(contains? % x) sample-pred-results))
         pxy (count (filter #(and (contains? % x) (contains? % y)) sample-pred-results))]
     (if (zero? px)
       0
       (/ pxy px)))))

(defn- get-game-pred-pairs [game]
  (for [x (take 3 game)
        y (drop 3 game)]
    [x y]))

(defn- remove-games-with-impossible-pairs [possible-games pred-pair-counts]
  (remove
    (fn [game]
      (some #(not (contains? pred-pair-counts %)) (get-game-pred-pairs game)))
    possible-games))

(defn- remove-games-with-redundant-pairs [possible-games sample-pred-results pred-pair-counts]
  (let [pred-pairs (keys pred-pair-counts)
        coincidence-rates (map (juxt identity (partial coincidence-rate sample-pred-results)) pred-pairs)
        redundant-pairs (->> coincidence-rates
                             (filter #(> (second %) max-coincidence-rate))
                             (map first))]
    (remove
      (fn [game]
        (some #(se/superset? (set game) (set %)) redundant-pairs))
      possible-games)))

(defn- remove-games-with-easy-pairs [possible-games pred-pair-counts sample-size]
  (let [easy-pairs (->> pred-pair-counts
                        (filter #(> (/ (val %) sample-size) max-answer-rate))
                        (map first))]
    (remove
      (fn [game]
        (some #(contains? (set (get-game-pred-pairs game)) %) easy-pairs))
      possible-games)))

(defn generate-game-preds
  "Generates game preds using the given backend."
  [backend]
  (loop [attempt 1]
    (let [pred-classes (or (core/get-pred-classes backend)
                           (throw (ex-info "Backend does not support pred gen" {})))
          sample (core/get-sample backend)
          pred-pool (repeatedly pred-pool-size #(generate-pred backend (rand-nth pred-classes) sample))
          sample-pred-results (map (fn [entity]
                                     (->> pred-pool
                                          (filter #(core/passes? backend % entity))
                                          (into #{})))
                                   sample)
          pred-pair-counts (->> sample-pred-results
                                (map #(combo/combinations % 2))
                                (filter some?)
                                (apply concat)
                                (frequencies))
          possible-games (combo/combinations pred-pool 6)
          candidate-games possible-games
          candidate-games (remove-games-with-impossible-pairs candidate-games pred-pair-counts)
          candidate-games (remove-games-with-redundant-pairs candidate-games sample-pred-results pred-pair-counts)
          candidate-games (remove-games-with-easy-pairs candidate-games pred-pair-counts (count sample))]
      (if (seq candidate-games)
        (rand-nth candidate-games)
        (if (< attempt max-attempts)
          (recur (inc attempt))
          (throw (ex-info "Too many attempts" {})))))))

(defonce game-cache (atom {}))

(defn generate-and-cache
  "Generates game preds using the given backend and caches them for fast
  retrieval."
  [backend ns game-id]
  (if-let [game (get-in @game-cache [ns game-id])]
    game
    (let [game (generate-game-preds backend)]
      (swap! game-cache assoc ns {game-id game})
      game)))
