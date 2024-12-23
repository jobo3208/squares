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

(def familiar-manufacturers
  ["Discraft" "Dynamic Discs" "Innova" "Latitude 64" "Westside Discs"])

(def familiar-discs
  ["Aviar" "Cookie" "Corvette" "Destroyer" "Escape" "Fuse" "Invader"
   "Jade" "Judge" "Leopard" "Mako3" "Pig" "Pure" "Recoil" "Roc" "Sensei"
   "Stag" "Tern" "Thunderbird" "Wraith" "Zone"])

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

(defmethod generate-pred ::name-starts-with [_ sample]
  (let [v (rand-nth (map (comp first :title) sample))]
    {:fn #(= ((comp first :title) %) v) :name (str "Starts with " v)}))

(defmethod generate-pred ::manufactured-by [_ _]
  (let [v (rand-nth familiar-manufacturers)]
    {:fn #(= (:brand %) v) :name v}))

(defmethod generate-pred ::compare-stability [_ _]
  (let [candidates (->> data
                        vals
                        (filter #((set familiar-discs) (:title %)))
                        (filter :stability))
        disc (rand-nth candidates)
        op (rand-nth ['< '>])]
    {:fn (fn [d]
           (when-let [stability (:stability d)]
             ((resolve op) stability (:stability disc))))
     :name (str (if (= op '<) "Less" "More") " stable than " (:title disc))}))

(defmethod generate-pred ::broad-category [_ sample]
  (let [v (rand-nth (map (comp broad-category :category) sample))]
    {:fn #(= ((comp broad-category :category) %) v) :name (string/capitalize (name v))}))

(defmethod generate-pred ::compare-flight-number [_ sample]
  (let [number (rand-nth [:speed :glide :turn :fade])
        v (rand-nth (map number sample))
        op (rand-nth ['< '>])]
    {:fn #((resolve op) (number %) v)
     :name (str (string/capitalize (name number)) " " op " " v)}))

(defrecord DiscsBackend []
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

(defmethod get-backend 'squares.backend.discs [_ _]
  (->DiscsBackend))
