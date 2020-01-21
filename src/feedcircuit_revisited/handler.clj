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
    (if-let [[_ ord-num feed] (re-matches #"([0-9]+),(.*)" item-id)]
      [feed (as-int ord-num)]
      item-id)))

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
       (html/html (ui/build-unread user-id
                                 (or (as-int count) ui/page-size))))

  (GET "/selected" {user-id :user}
       (html/html (ui/build-selected user-id)))

  (POST "/selected" {user-id :user {id "id"} :form-params}
        (->> (ensure-coll id)
             (map parse-item-id)
             (feed/selected-add! user-id)))

  (DELETE "/selected" {user-id :user {id :id} :params}
          (->> (ensure-coll id)
               (map parse-item-id)
               (feed/selected-remove! user-id)))

  (POST "/positions" {user-id :user {np "next-position"} :form-params}
        (let [positions (map parse-item-id (ensure-coll np))]
          (ui/mark-read user-id positions)
          {:status 303 :headers {"Location" "/"}}))

  (GET "/feed" {user-id :user
                {url :url
                 from :from
                 count :count} :params}
       (html/html (ui/build-feed user-id url (as-int from) (as-int count))))

  (GET "/settings" {user-id :user}
       (html/html (ui/build-settings user-id)))

  (POST "/settings" {user-id :user
                          {feeds "feeds"} :form-params}
        (ui/save-settings user-id feeds)
        {:status 303 :headers {"Location" "/"}})

  (GET "/subscribe" {user-id :user
                     {url :url} :params}
       (let [user (feed/get-user-attrs user-id)]
         (if-not (@feed/feed-dir url)
           (feed/add-feed! url))
         (feed/update-user-attrs! (update user :feeds conj url))
         {:status 303 :headers {"Location" "/"}}))

  (GET "/debug" request (str request))

  (GET "/extra-links" {user-id :user}
       (html/html (ui/build-extra-links user-id))))

(defroutes public-routes
  (GET "/login-options" []
       (html/html (ui/build-login-options)))

  (GET "/plain" {{url :url source :source} :params}
       (let [iid (parse-item-id url)
             [feed ord-num] (when (coll? iid) iid)
             url (when (string? iid) iid)
             result (ui/build-content feed ord-num url source)]
         (if (string? result)
           {:status 302 :headers {"Location" result}}
           (html/html result))))

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

  (GET "/logout" []
       {:status 302
        :headers {"Location" "/"}
        :session nil})

  (route/resources "/"))

(defn create []
  (wrap-defaults
   (routes public-routes
           (wrap-auth protected-routes)
           (route/not-found "No such resource"))
   (-> site-defaults
       (assoc-in [:security :anti-forgery] false)
       (assoc-in [:session :store] (cookie-store {:key (conf/param :cookie-key)})))))
