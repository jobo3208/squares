(ns squares.web
  (:require [clojure.edn :as edn]
            [clojure.spec.alpha :as s]
            [hiccup.core :refer [html]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.cookies :refer [wrap-cookies]]
            [ring.middleware.flash :refer [wrap-flash]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.util.response :as resp]
            [squares.config :as conf]
            [squares.core :as core]))

(defn- ans-td [square-idx answer finished]
  (if answer
    [:td {:style "background: #ddd"} answer]
    [:td
     [:form {:method :post :autocomplete :off}
      [:input {:type :hidden :name "square-idx" :value square-idx}]
      [:input {:type :text :name "entity-id" :disabled finished}]
      [:input {:type :submit :value ">" :disabled finished}]]]))

(defn- grid-tpl [pred-strs ans-strs guesses-left message]
  (let [finished (zero? guesses-left)
        ans-td #(ans-td % (nth ans-strs %) finished)]
    (html
      [:html
       [:head
        [:style "
        .bordered-table {
          border-collapse: collapse;
        }
        .bordered-table th,
        .bordered-table td {
          border: 1px solid #000;
          padding: 8px;
        }"]]
       [:body
        [:p#message message]
        [:table.bordered-table
         [:tr [:th] [:th (nth pred-strs 0)] [:th (nth pred-strs 1)] [:th (nth pred-strs 2)]]
         [:tr [:th (nth pred-strs 3)] (ans-td 0) (ans-td 1) (ans-td 2)]
         [:tr [:th (nth pred-strs 4)] (ans-td 3) (ans-td 4) (ans-td 5)]
         [:tr [:th (nth pred-strs 5)] (ans-td 6) (ans-td 7) (ans-td 8)]]
        [:p "Guesses left:" guesses-left]]])))

(defn- grid-view [backend state message]
  (let [preds (core/get-preds backend (:game-id state))
        pred-strs (map (partial core/display-pred backend) preds)
        ans-strs (map #(when %
                         (core/display-entity backend (core/get-entity backend %)))
                      (:answers state))]
    (grid-tpl pred-strs ans-strs (:guesses-left state) message)))

(defn- parse-guess [params]
  (let [raw-guess (select-keys params ["square-idx" "entity-id"])]
    (when (= (count raw-guess) 2)
      (-> raw-guess
          (update-keys keyword)
          (update :square-idx #(Integer. %))))))

(defn- create-handler [config]
  (fn handler [{:keys [uri cookies request-method params flash]}]
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
                          (parse-guess params)
                          (catch NumberFormatException _
                            nil))]
              (if (s/valid? ::core/guess guess)
                (let [[state' result] (core/make-guess state backend guess)
                      message (case result
                                :correct "Correct!"
                                :incorrect "Sorry, that's incorrect."
                                :dupe-guess "You've already used that answer."
                                :no-guesses-left "Game over."
                                nil)]
                  (-> (resp/redirect uri :see-other)
                      (assoc :flash message)
                      (assoc-in [:cookies "state"] {:path uri
                                                    :max-age (* 24 60 60)
                                                    :value (str state')})))
                (resp/bad-request "Guess is malformed.")))
            (resp/response (str (grid-view backend state flash)))))
        (resp/not-found "No matching backend found.")))))

(defn create-app [config]
  (-> (create-handler config)
      wrap-flash
      wrap-session
      wrap-cookies
      wrap-params))

(defn -main [& _]
  (let [config (conf/load-config! "squares.edn")
        app (create-app config)]
    (run-jetty app {:port 3000})))

(comment
  ; for repl
  (let [config (conf/load-config! "squares.edn")
        app (create-app config)]
    (def server (run-jetty app {:port 3000 :join? false}))))
