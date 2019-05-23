(ns feedcircuit-revisited.handler
  (:require [compojure.core :refer :all]
            [compojure.coercions :refer [as-int]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [hiccup.core :as html]
            [java-time :as jt]
            [feedcircuit-revisited.ui :as ui]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.auth :as auth]))

(defn parse-item-id [item-id]
  (if item-id
    (let [[_ ord-num feed] (re-matches #"([0-9]+),(.*)" item-id)]
      [feed (as-int ord-num)])))

(defn ensure-coll [x]
  (cond
    (coll? x) x
    x [x]))

(defn redirect-to-login [url]
  {:session nil :status 302 :headers {"Location" url}})

(defn wrap-auth [handler]
  (fn [request]
    (let [{user-id :user
           expires :expires
           via :via} (get-in request [:session])]
      (if (and user-id via)
        (if (and expires (> expires (jt/to-millis-from-epoch (jt/instant))))
          (handler (assoc request :user user-id))
          (redirect-to-login (auth/get-provider-url via
                                                    {:login_hint user-id
                                                     :state (:uri request)})))
        (redirect-to-login "login-options")))))

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
          {:status 303 :headers {"Location" "/"}}))

  (POST "/archive" {user-id :user
                    {items "selected-item"} :form-params}
        (ui/archive-items user-id
                          (map as-int (ensure-coll items)))
        {:status 303 :headers {"Location" "selected"}})

  (GET "/settings" {user-id :user}
       (html/html (ui/build-settings user-id)))

  (POST "/save-settings" {user-id :user
                          {feeds "feeds"} :form-params}
        (ui/save-settings user-id feeds)
        {:status 303 :headers {"Location" "/"}}))

(defroutes public-routes
  (GET "/login-options" []
       (html/html (ui/build-login-options)))

  (GET "/authenticate/:via" {{via :via code :code} :params}
       (if-let [user-id (auth/get-email via code)]
         {:status 302
          :headers {"Location" "/"}
          :session {:user user-id
                    :via via
                    :expires (+ (jt/to-millis-from-epoch (jt/instant))
                                (int 1e9))}
          :session-cookie-attrs {:expires "Wed, 11 Nov 2111 11:11:11 GMT"
                                 :same-site :lax}}

         {:status 403}))

  (route/resources "/"))

(defn create []
  (wrap-defaults
   (routes public-routes
           (wrap-auth protected-routes)
           (route/not-found "No such resource"))
   (-> site-defaults
       (assoc-in [:security :anti-forgery] false)
       (assoc-in [:session :store] (cookie-store {:key (conf/param :cookie-key)})))))
