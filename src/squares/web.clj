(ns squares.web
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [compojure.core :refer [GET POST context routes wrap-routes]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
            [squares.config :as conf]
            [squares.core :as core]
            [squares.web.util :refer [abort html-resp wrap-abort wrap-swallow-exceptions]]))

(defn- index-tpl [config]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
    [:title "squares"]
    [:link {:rel :stylesheet :href "/css/styles.css?v=3"}]]
   [:body
    [:div.container
     [:h1 "squares"]
     [:p "Select a grid to play:"]
     [:ul
      (for [backend-name (sort (keys config))]
        [:li [:a {:href backend-name} backend-name]])]]]])

(defn- index-view [config request]
  (html-resp (index-tpl config)))

(defn- ans-div [backend-name square-idx answer guess result finished]
  (let [show-feedback? (= square-idx (:square-idx guess))]
    (if answer
      [:div.grid-item
       (when (and show-feedback? (= result :correct))
         [:div.feedback.good
          [:img {:src "/icons/check-circle.svg"}]])
       [:div.grid-item-text answer]]
      [:div.grid-item
       (if finished
         {:class "finished"}
         {:hx-get (str "/" backend-name "/select")
          :hx-include :this
          :hx-target :body})
       (when show-feedback?
         (case result
           :incorrect
           [:div.feedback.bad
            [:img {:src "/icons/x-circle.svg"}]]
           :unknown-entity
           [:div.feedback.neutral
            [:img {:src "/icons/help-circle.svg"}]]
           :dupe-guess
           [:div.feedback.neutral
            [:img {:src "/icons/slash.svg"}]]))
       (when (not finished)
         [:form
          [:input {:type :hidden :name "square-idx" :value square-idx}]])])))

(defn- game-tpl [backend-name backend state guess result select]
  (let [preds (core/get-preds backend (:game-id state))
        pred-strs (map (partial core/display-pred backend) preds)
        ans-strs (map #(when %
                         (core/display-entity backend (core/get-entity backend %)))
                      (:answers state))
        finished (zero? (:guesses-left state))
        ans-div #(ans-div backend-name % (nth ans-strs %) guess result finished)
        searchable (satisfies? core/Search backend)]
    [:div.container
      [:div.grid-container
       (when select
         [:div.search
          [:div.search-bar
           (if searchable
             [:input {:type :text
                      :placeholder "Search..."
                      :name :query
                      :hx-get (str "/" backend-name "/search")
                      :hx-trigger "input changed delay:500ms"
                      :hx-target ".search-results"
                      :autocomplete :off
                      :autofocus true}]
             [:form {:hx-post (str "/" backend-name "/guess")
                     :hx-target :body
                     :hx-include "#search-square-idx"}
              [:input {:type :text
                       :placeholder "Enter a guess"
                       :name :entity-id
                       :autocomplete :off
                       :autofocus true}]
              [:input {:type :submit :value "Guess"}]])
           [:input#search-square-idx {:type :hidden :name "square-idx" :value select}]]
          [:div.search-results]
          [:div.search-backdrop
           {:hx-get (str "/" backend-name "/select")
            :hx-trigger "click, keyup[key=='Escape'] from:body"
            :hx-target :body}]])
       [:div.heading]
       [:div.heading (nth pred-strs 0)]
       [:div.heading (nth pred-strs 1)]
       [:div.heading (nth pred-strs 2)]
       [:div.heading (nth pred-strs 3)]
       (ans-div 0)
       (ans-div 1)
       (ans-div 2)
       [:div.heading (nth pred-strs 4)]
       (ans-div 3)
       (ans-div 4)
       (ans-div 5)
       [:div.heading (nth pred-strs 5)]
       (ans-div 6)
       (ans-div 7)
       (ans-div 8)]
      [:div.guesses-left
       [:h1 (:guesses-left state)]]]))

(defn- game-page-tpl [backend-name backend state]
  [:html
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:name :viewport :content "width=device-width, initial-scale=1"}]
    [:title (str backend-name " - squares")]
    [:script {:src "/js/htmx.min.js"}]
    [:script {:src "/js/squares.js"}]
    [:link {:rel :stylesheet :href "/css/styles.css?v=2"}]]
   [:body
    [:base {:href (str "/" backend-name)}]
    (game-tpl backend-name backend state nil nil nil)]])

(defn- parse-state [request]
  (-> request
      (get-in [:cookies "state" :value])
      (edn/read-string)))

(defn- get-state [current-game-id request]
  (let [input-state (parse-state request)]
    (if (or (nil? input-state)
            (not (s/valid? ::core/state input-state))
            (not= (:game-id input-state) current-game-id))
      (core/init-state current-game-id)
      input-state)))

(defn- game-page-view [config {:keys [::backend-name ::backend] :as request}]
  (let [current-game-id (core/get-current-game-id backend)
        state (get-state current-game-id request)]
    (html-resp (game-page-tpl backend-name backend state))))

(defn- select-view [config {:keys [::backend-name ::backend] :as request}]
  (let [square-idx (-> request :params :square-idx)
        current-game-id (core/get-current-game-id backend)
        state (get-state current-game-id request)]
    (html-resp (game-tpl backend-name backend state nil nil square-idx))))

(defn- search-view [config {:keys [::backend-name ::backend] :as request}]
  (let [query (-> request :params :query)
        results (if (> (count query) 2)
                  (core/search-entities backend query)
                  [])]
    (html-resp
      (when (seq results)
        [:ul
         (for [result results]
          [:li
           {:hx-post (str "/" backend-name "/guess")
            :hx-target :body
            :hx-include "#search-square-idx"
            :hx-vals (format "{\"entity-id\": \"%s\"}" result)}
           result])]))))

(defn- parse-guess [request]
  (let [str->int #(try (Integer. %) (catch NumberFormatException _ nil))
        guess {:square-idx (str->int (get-in request [:params :square-idx]))
               :entity-id (get-in request [:params :entity-id])}]
    (when (s/valid? ::core/guess guess)
      guess)))

(defn- guess-view [config {:keys [::backend-name ::backend] :as request}]
  (let [guess (or (parse-guess request) (abort 400 "Malformed guess"))
        current-game-id (core/get-current-game-id backend)
        state (get-state current-game-id request)
        [state' result] (core/make-guess state backend guess)]
    (-> (html-resp (game-tpl backend-name backend state' guess result nil))
        (assoc-in [:cookies "state"] {:path (str "/" backend-name)
                                      :max-age (* 24 60 60)
                                      :value (str state')})
        (assoc-in [:headers "HX-Trigger-After-Settle"] "guessMade"))))

(defn- wrap-squares-backend [handler config]
  (fn [request]
    (let [backend-name (-> request :params :backend-name)
          backend-config (or (config backend-name) (abort 404 "Unknown backend"))
          backend (core/get-backend (:ns backend-config) (:opts backend-config))
          request (assoc request ::backend-name backend-name
                                 ::backend backend)]
      (handler request))))

(defn- create-routes [config]
  (routes
    (GET "/" request (index-view config request))
    (wrap-routes
      (routes
        (GET "/:backend-name" request (game-page-view config request))
        (context "/:backend-name" []
          (GET "/select" request (select-view config request))
          (GET "/search" request (search-view config request))
          (POST "/guess" request (guess-view config request))))
      wrap-squares-backend config)))

(defn- create-app [config]
  (-> (create-routes config)
      (wrap-defaults (assoc-in site-defaults [:security :anti-forgery] false))
      (wrap-abort)))

(defn -main [& _]
  (let [config (conf/load-config! "squares.edn")
        app (-> (create-app config)
                (wrap-swallow-exceptions))]
    (run-jetty app {:port 3000})))

(comment
  ; for repl
  (let [config (conf/load-config! "squares.edn")
        handler (create-app config)]
    (def server (run-jetty handler {:host "0.0.0.0" :port 3000 :join? false})))

  (.stop server))
