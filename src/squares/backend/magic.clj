(ns squares.backend.magic
  "Magic: The Gathering cards

  opts:

  :db-path - (required) absolute path to an AllPrintings.sqlite file from mtgjson.com
  :familar-sets - (optional) vector of set codes familiar to audience"
  (:require [clojure.string :as string]
            [honey.sql :as sql]
            [java-time.api :as jt]
            [next.jdbc :as jdbc]
            [squares.core :refer [get-backend GridBackend Search PredGen]]
            [squares.pred-gen :refer [generate-and-cache generate-pred]]))

(def create-date (jt/local-date 2025 2 8))

(def pred-classes
  [::name-starts-with
   ::compare-converted-mana-cost
   ::rarity
   #_ ::color  ; too easy
   ::in-set
   ::compare-power-toughness
   ::has-type
   ::has-subtype
   ::exact-mana-cost])

(def entity-fields
  [:name
   :manaCost
   :manaValue
   :colorIdentity
   :types
   :subtypes
   :power
   :toughness
   :printings
   :rarity])

(def color-name
  {"W" "White"
   "U" "Blue"
   "G" "Green"
   "B" "Black"
   "R" "Red"})

(defn get-set-name [ds set-code]
  (->> (jdbc/execute! ds (sql/format {:select [:name]
                                      :from [:sets]
                                      :where [:= :code set-code]}))
       first
       :sets/name))

(defn cs->list [cs]
  (->> (string/split cs #", ")
       (filter seq)))

(defn cs->set [cs]
  (into #{} (cs->list cs)))

(defmethod generate-pred ::name-starts-with [_ _ sample]
  (let [v (rand-nth (map (comp first :cards/name) sample))]
    {:fn #(= ((comp first :cards/name) %) v)
     :sig [::name-starts-with v]
     :name (str "Starts with " v)}))

(defmethod generate-pred ::compare-converted-mana-cost [_ _ sample]
  (let [v (rand-nth (map (comp int :cards/manaValue) sample))
        op (rand-nth ['= '< '>])]
    {:fn #((resolve op) (:cards/manaValue %) v)
     :sig [::compare-converted-mana-cost op v]
     :name (str "Cost " op " " v)}))

(defmethod generate-pred ::exact-mana-cost [_ _ sample]
  (let [v (->> (map :cards/manaCost sample)
               (filter some?)
               (rand-nth))]
    {:fn #(= (:cards/manaCost %) v)
     :sig [::exact-mana-cost v]
     :name v}))

(defmethod generate-pred ::rarity [_ _ sample]
  (let [v (rand-nth (map :cards/rarity sample))]
    {:fn #(= (:cards/rarity %) v)
     :sig [::rarity v]
     :name (string/capitalize v)}))

(defmethod generate-pred ::color [_ _ _]
  (let [color (rand-nth (keys color-name))]
    {:fn #(contains? (cs->set (:cards/colorIdentity %)) color)
     :sig [::color color]
     :name (color-name color)}))

(defmethod generate-pred ::in-set [{:keys [ds opts]} _ sample]
  (let [sets (or (:familiar-sets opts)
                 (mapcat #(cs->list (:cards/printings %)) sample))
        set-code (rand-nth sets)]
    {:fn #(contains? (cs->set (:cards/printings %)) set-code)
     :sig [::in-set set-code]
     :name (get-set-name ds set-code)}))

(defn str->int [s]
  (try
    (Integer. s)
    (catch Exception _ nil)))

(defmethod generate-pred ::compare-power-toughness [_ _ sample]
  (let [measure (rand-nth [:cards/power :cards/toughness])
        v (->> sample
               (map (comp str->int measure))
               (filter some?)
               (rand-nth))
        op (rand-nth ['= '< '>])]
    {:fn #(when-let [arg (str->int (measure %))]
            ((resolve op) arg v))
     :sig [::compare-power-toughness op v]
     :name (str (string/capitalize (name measure)) " " op " " v)}))

(defmethod generate-pred ::has-type [_ _ sample]
  (let [card (rand-nth sample)
        type_ (rand-nth (cs->list (:cards/types card)))]
    {:fn #(contains? (cs->set (:cards/types %)) type_)
     :sig [::has-type type_]
     :name type_}))

(defmethod generate-pred ::has-subtype [_ _ sample]
  (let [card (->> sample
                  (filter (comp seq :cards/subtypes))
                  (rand-nth))
        subtype (rand-nth (cs->list (:cards/subtypes card)))]
    {:fn #(contains? (cs->set (:cards/subtypes %)) subtype)
     :sig [::has-subtype subtype]
     :name subtype}))

(defrecord MagicBackend [ds opts]
  GridBackend
  (get-current-game-id [_]
    (inc (jt/time-between create-date (jt/local-date) :days)))
  (get-preds [this game-id]
    (generate-and-cache this 'squares.backend.magic game-id))
  (get-entity [_ entity-id]
    (->> (jdbc/execute! ds (sql/format {:select entity-fields
                                        :from [:cards]
                                        :where [:= :name entity-id]}))
         first))
  (display-pred [_ pred]
    (:name pred))
  (display-entity [_ entity]
    (:cards/name entity))
  (passes? [_ pred entity]
    ((:fn pred) entity))

  Search
  (search-entities [_ query]
    (->> (jdbc/execute! ds (sql/format {:select [[[:distinct :name]]]
                                        :from [:cards]
                                        :where [:like :name [:concat query "%"]]}))
         (map :cards/name)
         (sort)))

  PredGen
  (get-sample [_]
    (jdbc/execute! ds (sql/format {:select entity-fields
                                   :from [:cards]
                                   :order-by [:%random]
                                   :limit 1000})))
  (get-pred-classes [_]
    pred-classes))

(defmethod get-backend 'squares.backend.magic [_ opts]
  (let [db-spec {:dbtype "sqlite"
                 :dbname (:db-path opts)}
        ds (jdbc/get-datasource db-spec)]
    (->MagicBackend ds opts)))
