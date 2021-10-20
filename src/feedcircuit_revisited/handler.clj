(ns feedcircuit-revisited.handler
  (:require [compojure.core :refer :all]
            [compojure.coercions :refer [as-int]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [hiccup core page]
            [java-time :as jt]
            [feedcircuit-revisited.ui :as ui]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.conf :as conf]
            [feedcircuit-revisited.auth :as auth]))

(defn html5 [subject]
  (hiccup.core/html (hiccup.page/doctype :html5) subject))

(defn parse-feed-postion [feed-pos]
  (if feed-pos
    (if-let [[_ ord-num feed] (re-matches #"([0-9]+),(.*)" feed-pos)]
      [feed (as-int ord-num)]
      feed-pos)))

(defn ensure-coll [x]
  (cond
    (coll? x) x
    x [x]))

(defn redirect-to-login [url]
  {:session nil :status 302 :headers {"Location" url}})

(defn wrap-auth-everlasting [handler]
  (fn [request]
    (if-let [user-id (get-in request [:session :user])]
      (handler (assoc request :user user-id))
      {:status 403})))

(defn alive? [expires-at]
  (and expires-at
       (> expires-at
          (jt/to-millis-from-epoch (jt/instant)))))

(defn wrap-only-user [handler]
  (fn [request]
    (let [{user-id :user
           expires-at :expires} (get-in request [:session])
          request (if (alive? expires-at)
                    (assoc request :user user-id)
                    request)]
      (handler request))))

(defn wrap-auth [handler]
  (fn [request]
    (let [{user-id :user
           expires-at :expires
           via :via} (get-in request [:session])]
      (if (and user-id via)
        (if (alive? expires-at)
          (handler (assoc request :user user-id))
          (redirect-to-login (auth/get-provider-url via
                                                    {:login_hint user-id
                                                     :state (:uri request)})))
        (redirect-to-login "login-options")))))

(defroutes non-interactive-routes
  (GET "/selected" {user-id :user
                    {{extra-style :value} "extra-style"} :cookies}
       (html5 (ui/build-selected user-id extra-style)))

  (POST "/selected" {user-id :user {id "id"} :form-params}
        (->> (ensure-coll id)
             (feed/selected-add! user-id)))

  (DELETE "/selected" {user-id :user {id :id} :params}
          (->> (ensure-coll id)
               (feed/selected-remove! user-id))))

(defroutes protected-routes
  (GET "/" {user-id :user
            {count :count} :params
            {{extra-style :value} "extra-style"} :cookies}
       (html5 (ui/build-unread user-id
                                   (or (as-int count) ui/page-size)
                                   extra-style)))

  (POST "/next" {user-id :user {np "next-position"
                                selected "selected-item"} :form-params}
        (let [positions (map parse-feed-postion (ensure-coll np))
              items     (ensure-coll selected)]
          (feed/selected-add! user-id items)
          (ui/mark-read user-id positions)
          {:status 303 :headers {"Location" "/"}}))

  (POST "/complete-selected" {user-id :user
                              {selected "selected-item"} :form-params}
        (feed/selected-remove! user-id (ensure-coll selected))
        {:status 303 :headers {"Location" "selected"}})

  (GET "/feed" {user-id :user
                {url :url
                 from :from
                 count :count} :params
                {{extra-style :value} "extra-style"} :cookies}
       (html5 (ui/build-feed user-id
                                 url
                                 (as-int from)
                                 (as-int count)
                                 extra-style)))

  (GET "/sources" {user-id :user
                   {{extra-style :value} "extra-style"} :cookies}
       (html5 (ui/build-sources user-id extra-style)))

  (GET "/settings" {user-id :user
                    {{extra-style :value} "extra-style"} :cookies}
       (html5 (ui/build-settings user-id extra-style)))

  (POST "/settings" {user-id :user
                     {feeds "feeds"
                      styles "styles"
                      extra-style "extra-style"} :form-params}
        (ui/save-settings user-id feeds styles)
        {:status 303
         :headers {"Location" "/"}
         :cookies {"extra-style" {:value extra-style
                                  :expires "Wed, 11 Nov 2111 11:11:11 GMT"}}})

  (GET "/subscribe" {user-id :user
                     {url :url} :params}
       (if-not (@feed/feed-index url)
         (feed/add-feed! url))
       (feed/update-user-attrs! user-id update :feeds conj url)
       {:status 303 :headers {"Location" "/"}})

  (GET "/debug" request (str request))

  (GET "/extra-links" {user-id :user
                       {{extra-style :value} "extra-style"} :cookies}
       (html5 (ui/build-extra-links user-id extra-style))))

(defroutes public-routes
  (GET "/login-options" {{{extra-style :value} "extra-style"} :cookies}
       (html5 (ui/build-login-options extra-style)))

  (GET "/plain" {user-id :user
                 {url :url source :source} :params
                 {{extra-style :value} "extra-style"} :cookies}
       (let [result (ui/build-content url
                                      (= source "selected")
                                      extra-style
                                      user-id)]
         (if (string? result)
           {:status 302 :headers {"Location" result}}
           (html5 result))))

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
   (routes (wrap-only-user public-routes)
           (wrap-auth protected-routes)
           (wrap-auth-everlasting non-interactive-routes)
           (route/not-found "No such resource"))
   (-> site-defaults
       (assoc-in [:security :anti-forgery] false)
       (assoc-in [:session :store] (cookie-store {:key (conf/param :cookie-key)})))))
