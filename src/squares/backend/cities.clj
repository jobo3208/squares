(ns squares.backend.cities
  (:require [clojure.java.io :as io]
            [java-time.api :as jt]
            [next.jdbc :as jdbc]
            [squares.core :refer [get-backend GridBackend]])
  (:import (java.util Random)))

(def create-date (jt/local-date 2024 1 13))

(defn city-in-state? [ds state city]
  (let [states (->> (jdbc/execute! ds ["select state from city where name = ?" city])
                    (map :city/state)
                    (into #{}))]
    (contains? states state)))

(defn shuffle'
  "Copy of core/shuffle that takes an RNG."
  [^java.util.Collection coll ^java.util.Random rng]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al rng)
    (clojure.lang.RT/vector (.toArray al))))

(def states
  ["AK" "AL" "AR" "AZ" "CA" "CO" "CT" "DE" "FL" "GA" "HI" "IA" "ID" "IL" "IN"
   "KS" "KY" "LA" "MA" "MD" "ME" "MI" "MN" "MO" "MS" "MT" "NC" "ND" "NE" "NH"
   "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VA"
   "VT" "WA" "WI" "WV" "WY"])

(defrecord CitiesBackend [ds]
  GridBackend
  (get-current-game-id [_]
    (inc (jt/time-between create-date (jt/local-date) :days)))
  (get-preds [_ game-id]
    (let [rng (Random. game-id)
          letters (take 3 (shuffle' (vec "ABCDEFGHIJKLMNOPRSTUVW") rng))
          states (take 3 (shuffle' states rng))
          ->letter-pred (fn [letter]
                          {:name (str letter) :fn #(= (first %) letter)})
          ->state-pred (fn [state]
                         {:name state :fn (partial city-in-state? ds state)})]
      (concat
        (map ->letter-pred letters)
        (map ->state-pred states))))
  (searchable? [_] false)
  (search-entities [_ _]
    nil)
  (get-entity [_ entity-id]
    entity-id)
  (display-pred [_ pred]
    (:name pred))
  (display-entity [_ entity]
    entity)
  (passes? [_ pred entity]
    ((:fn pred) entity)))

(defmethod get-backend 'squares.backend.cities [_ _]
  (let [db-spec {:dbtype "sqlite"
                 :dbname (io/resource "cities.db")}
        ds (jdbc/get-datasource db-spec)]
    (->CitiesBackend ds)))
