(ns squares.web.util
  (:require [hiccup.core :refer [html]]
            [ring.util.response :as resp]))

(defn html-resp
  "Return response with given HTML body. body can be a string or hiccup vector."
  [body]
  (let [body (if (vector? body) (html body) body)]
    (-> (resp/response (str body))
        (assoc-in [:headers "Content-Type"] "text/html"))))

(defn abort
  "Abort current execution and respond immediately with given status and
  optional message. Intended to mirror Flask's abort."
  ([status]
   (abort status ""))
  ([status message]
   (throw (ex-info "Abort" {:abort/status status :abort/message message}))))

(defn wrap-abort
  "Middleware to detect use of abort function and respond accordingly."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Throwable e
        (if-let [status (:abort/status (ex-data e))]
          {:status status
           :headers {}
           :body (:abort/message (ex-data e))}
          (throw e))))))

(defn wrap-swallow-exceptions
  "Middleware to swallow exceptions and respond with a generic 500 error.
  Intended for production."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch Throwable _ {:status 500 :headers {} :body "Internal Server Error"}))))
