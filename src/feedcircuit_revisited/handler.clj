(ns feedcircuit-revisited.handler
  (:require [compojure.core :refer :all]
            [compojure.coercions :refer [as-int]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [hiccup.core :as html]
            [clojure.core.memoize :as memz]
            [feedcircuit-revisited.ui :as ui]
            [feedcircuit-revisited.feed :as feed]))

(defn parse-item-id [item-id]
  (if item-id
    (let [[_ ord-num feed] (re-matches #"([0-9]+),(.*)" item-id)]
      [feed (as-int ord-num)])))

(defn ensure-coll [x]
  (cond
    (coll? x) x
    x [x]))

(defn wrap-auth [handler]
  (fn [request]
    (if-let [user-id (get-in request [:session :user])]
      (handler (assoc request :user user-id))
      {:status 302 :headers {"Location" "login-options"}})))

(defroutes protected-routes
  (GET "/" {user-id :user {count :count} :params}
       (html/html (ui/build-feed user-id
                                 (or (as-int count) ui/page-size))))

  (GET "/selected" {user-id :user}
       (html/html (ui/build-selected user-id)))

  (GET "/read" {user-id :user {id :id url :url} :params}
       (let [[feed item-id] (parse-item-id id)
             result (ui/build-content user-id item-id feed url)]
         (if (string? result)
           {:status 302 :headers {"Location" result}}
           (html/html result))))

  (POST "/next" {user-id :user
                 {np "next-position"
                  si "selected-item"} :form-params}
        (let [positions (map parse-item-id (ensure-coll np))
              selected-ids (map parse-item-id (ensure-coll si))]
          (ui/mark-read user-id positions selected-ids)
          {:status 302 :headers {"Location" "/"}}))

  (POST "/archive" {user-id :user
                    {items "selected-item"} :form-params}
        (ui/archive-items user-id
                          (map as-int (ensure-coll items)))
        {:status 302 :headers {"Location" "selected"}})

  (GET "/settings" {user-id :user}
       (html/html (ui/build-settings user-id)))

  (POST "/save-settings" {user-id :user
                          {feeds "feeds"} :form-params}
        (ui/save-settings user-id feeds)
        {:status 302 :headers {"Location" "/"}}))

(defroutes public-routes
  (GET "/login-options" []
       "Here you'll be presented with login options")

  (GET "/authenticate/:via" {{via :via user :user000} :params}
       (if (and (= via "backdoor000") user)
         {:status 302
          :headers {"Location" "/"}
          :session {:user user}}
         {:status 403}))

  (route/resources "/"))

(def app
  (wrap-defaults
   (routes public-routes
           (wrap-auth protected-routes)
           (route/not-found "No such resource"))
   (-> site-defaults
       (assoc-in [:security :anti-forgery] false)
       (assoc-in [:session :store] (cookie-store {:key "here goes a key "})))))

; === Debugging convenience functions ===

(defn _drop-cache []
  (reset! feed/dir-cache {})
  (memz/memo-clear! feed/get-data))

(use 'ring.adapter.jetty)

(defn _run-srv []
  (reset! feed/feed-dir (feed/load-feed-dirs))
  (run-jetty app {:port 8080 :join? false}))
