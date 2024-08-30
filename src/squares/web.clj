(ns squares.web
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [hiccup.core :refer [html]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as resp]
            [squares.config :as conf]
            [squares.core :as core]))

(defn- ans-div [square-idx answer finished]
  (if answer
    [:div.grid-item [:div.grid-item-text answer]]
    [:div.grid-item {:hx-prompt "Enter a guess:"
                     :hx-post ""
                     :hx-include "this"
                     :hx-target "body"}
     [:form
      [:input {:type :hidden :name "square-idx" :value square-idx}]]]))

(defn- grid-tpl [backend state result]
  (let [preds (core/get-preds backend (:game-id state))
        pred-strs (map (partial core/display-pred backend) preds)
        ans-strs (map #(when %
                         (core/display-entity backend (core/get-entity backend %)))
                      (:answers state))
        finished (zero? (:guesses-left state))
        ans-div #(ans-div % (nth ans-strs %) finished)
        [message disposition] (case result
                                :correct ["Correct!" :good]
                                :incorrect ["Sorry, that's incorrect." :bad]
                                :unknown-entity ["That's not a valid guess." :neutral]
                                :dupe-guess ["You've already used that answer." :neutral]
                                :no-guesses-left ["Game over." :bad]
                                [nil nil])]
    [:div.container
      (when message
        [:div#status
         {:class [:status disposition]}
         [:p message]])
      [:div.grid-container
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

(defn- page-tpl [backend state result]
  [:html
   [:head
    [:script {:src "/js/htmx.min.js"}]
    [:script {:src "/js/squares.js"}]
    [:link {:rel :stylesheet :href "/css/styles.css?v=2"}]]
   [:body
    (grid-tpl backend state result)]])

(defn- parse-guess [request]
  (when-let [square-idx (get-in request [:params "square-idx"])]
    (when-let [entity-id (get-in request [:headers "hx-prompt"])]
      {:square-idx (Integer. square-idx)
       :entity-id entity-id})))

(defn- create-handler [config]
  (fn handler [{:keys [uri cookies request-method params] :as request}]
    (let [backend-name (subs uri 1)]
      (if-let [backend-config (config backend-name)]
        (let [backend (core/get-backend (:ns backend-config) (:opts backend-config))
              input-state (-> cookies
                              (get-in ["state" :value])
                              (edn/read-string))
              current-game-id (core/get-current-game-id backend)
              state (if (or (nil? input-state)
                            (not (s/valid? ::core/state input-state))
                            (not= (:game-id input-state) current-game-id))
                      (core/init-state current-game-id)
                      input-state)]
          (if (= request-method :post)
            (let [guess (try
                          (parse-guess request)
                          (catch NumberFormatException _
                            nil))]
              (if (s/valid? ::core/guess guess)
                (let [[state' result] (core/make-guess state backend guess)]
                  (-> (resp/response (str (html (grid-tpl backend state' result))))
                      (assoc-in [:cookies "state"] {:path uri
                                                    :max-age (* 24 60 60)
                                                    :value (str state')})))
                (resp/bad-request "Guess is malformed.")))
            (resp/response (str (html (page-tpl backend state nil))))))
        (resp/not-found "No matching backend found.")))))

(defn create-app [config]
  (-> (create-handler config)
      (wrap-resource "public")
      (wrap-cookies)
      (wrap-params)))

(defn- wrap-swallow-exceptions [handler]
  (fn [request]
    (try
      (handler request)
      (catch Throwable _ {:status 500 :headers {} :body "Internal Server Error"}))))

(defn -main [& _]
  (let [config (conf/load-config! "squares.edn")
        app (-> (create-app config)
                wrap-swallow-exceptions)]
    (run-jetty app {:port 3000})))

(comment
  ; for repl
  ; TODO: changes in handler code are not automatically picked up!
  ; we have to use #' somewhere
  (let [config (conf/load-config! "squares.edn")
        app (create-app config)]
    (def server (run-jetty app {:port 3000 :join? false})))

  (.stop server))
