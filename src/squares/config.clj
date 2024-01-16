(ns squares.config
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]))

(s/def :squares.config.backend/name string?)
(s/def :squares.config.backend/ns symbol?)
(s/def :squares.config.backend/opts any?)
(s/def :squares.config.backend/config (s/keys :req-un [:squares.config.backend/ns]
                                              :opt-un [:squares.config.backend/opts]))
(s/def ::config (s/map-of :squares.config.backend/name
                          :squares.config.backend/config))

(defn- require-backends! [config]
  (dorun (map (comp require :ns val) config)))

(defn load-config!
  "Read config from filepath. Assert that config is valid, then require all
  backend namespaces within. Finally, return config."
  [filepath]
  (let [config (-> (slurp filepath) (edn/read-string))]
    (s/assert ::config config)
    (require-backends! config)
    config))
