(ns squares.core
  (:require [clojure.spec.alpha :as s]))

(defprotocol GridBackend
  (get-current-game-id
    [this]
    "Returns current game id. Usually produces a new game-id each day.")
  (get-preds
    [this game-id]
    "Returns sequence of predicates for given game. First 3 are the \"across\"
    predicates, latter 3 are the \"down\" predicates.")
  (searchable?
    [this]
    "Is this backend searchable? If not, the player-entered strings must serve
    as the entity-ids.")
  (search-entities
    [this query]
    "Returns a sequence of entities partially matching the given query string.
    Returns nil if backend is not searchable.")
  (get-entity
    [this entity-id]
    "Returns an entity given the entity-id.")
  (display-pred
    [this pred]
    "Returns a string representation of the predicate.")
  (display-entity
    [this entity]
    "Returns a string representation of the entity.")
  (passes?
    [this pred entity]
    "Does the entity satisfy the predicate?")
  (get-sample
    [this]
    "Returns a sample of entities. Used for pred gen.")
  (get-pred-classes
    [this]
    "Returns a sequence of pred classes. Used for pred gen."))

(defmulti get-backend
  "Create and return an instance of the backend at ns."
  (fn [ns opts]
    ns))

(s/def ::entity-id some?)
(s/def ::game-id pos-int?)
(s/def ::answers (s/coll-of (s/nilable ::entity-id) :kind vector? :count 9))
(s/def ::guesses-left (s/int-in 0 10))
(s/def ::state (s/keys :req-un [::game-id ::answers ::guesses-left]))

(defn init-state [game-id]
  {:game-id game-id
   :answers [nil nil nil nil nil nil nil nil nil]
   :guesses-left 9})

(s/def ::square-idx (s/int-in 0 10))
(s/def ::guess (s/keys :req-un [::square-idx ::entity-id]))

(defn make-guess
  "Make a guess. Returns a pair of the new state and one of the following status
  keywords: :correct, :incorrect, :dupe-guess, :no-guesses-left."
  [state backend guess]
  (let [{:keys [square-idx entity-id]} guess]
    (cond
      (zero? (:guesses-left state)) [state :no-guesses-left]
      (some #{entity-id} (:answers state)) [state :dupe-guess]
      :else
      (let [preds (get-preds backend (:game-id state))
            p1 (nth preds (mod square-idx 3))
            p2 (nth preds (+ 3 (quot square-idx 3)))
            entity (get-entity backend entity-id)
            state (update state :guesses-left dec)]
        (if (and (passes? backend p1 entity) (passes? backend p2 entity))
          [(assoc-in state [:answers square-idx] entity-id) :correct]
          [state :incorrect])))))
