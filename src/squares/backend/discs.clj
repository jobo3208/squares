(ns squares.backend.discs
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [java-time.api :as jt]
            [squares.core :refer [get-backend GridBackend PredGen]]
            [squares.pred-gen :refer [generate-and-cache generate-pred]]))

(def create-date (jt/local-date 2024 8 23))

(def data (-> (io/resource "flightguide.edn")
              (slurp)
              (edn/read-string)
              (->> (map (juxt :title identity))
                   (into {}))))

(defn broad-category [category]
  (cond
    (#{"Putters" "Approach"} category) :putter
    (= "Midrange Drivers" category) :midrange
    :else :driver))

(def pred-classes
  [::name-starts-with
   ::manufactured-by
   ::compare-stability
   ::broad-category
   ::compare-flight-number])

(defmethod generate-pred ::name-starts-with [_ _ sample]
  (let [v (rand-nth (map (comp first :title) sample))]
    {:fn #(= ((comp first :title) %) v) :name (str "Starts with " v)}))

(defmethod generate-pred ::manufactured-by [backend _ sample]
  (let [manufacturers (or (-> backend :opts :familiar-manufacturers)
                          (map :brand sample))
        v (rand-nth manufacturers)]
    {:fn #(= (:brand %) v) :name v}))

(defmethod generate-pred ::compare-stability [backend _ _]
  (let [candidates (->> data vals (filter :stability))
        familiar-discs (-> backend :opts :familiar-discs)
        candidates (if familiar-discs
                     (filter #((set familiar-discs) (:title %)) candidates)
                     candidates)
        disc (rand-nth candidates)
        op (rand-nth ['< '>])]
    {:fn (fn [d]
           (when-let [stability (:stability d)]
             ((resolve op) stability (:stability disc))))
     :name (str (if (= op '<) "Less" "More") " stable than " (:title disc))}))

(defmethod generate-pred ::broad-category [_ _ sample]
  (let [v (rand-nth (map (comp broad-category :category) sample))]
    {:fn #(= ((comp broad-category :category) %) v) :name (string/capitalize (name v))}))

(defmethod generate-pred ::compare-flight-number [_ _ sample]
  (let [number (rand-nth [:speed :glide :turn :fade])
        v (rand-nth (map number sample))
        op (rand-nth ['< '>])]
    {:fn #((resolve op) (number %) v)
     :name (str (string/capitalize (name number)) " " op " " v)}))

(defrecord DiscsBackend [opts]
  GridBackend
  (get-current-game-id [_]
    (inc (jt/time-between create-date (jt/local-date) :days)))
  (get-preds [this game-id]
    (generate-and-cache this 'squares.backend.discs game-id))
  (get-entity [_ entity-id]
    (get data entity-id))
  (display-pred [_ pred]
    (:name pred))
  (display-entity [_ entity]
    (:title entity))
  (passes? [_ pred entity]
    ((:fn pred) entity))

  PredGen
  (get-sample [_]
    (->> (shuffle (keys data))
         (take 100)
         (map (partial get data))))
  (get-pred-classes [_]
    pred-classes))

(defmethod get-backend 'squares.backend.discs [_ opts]
  (->DiscsBackend opts))
