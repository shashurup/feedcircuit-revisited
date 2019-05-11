(ns feedcircuit-revisited.handler
  (:require [compojure.core :refer :all]
            [compojure.coercions :refer [as-int]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
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

(defn get-user-id [] "georgy@kibardin.name")

(defroutes app-routes
  (GET "/" [count]
       (html/html (ui/build-feed (get-user-id)
                              (or (as-int count) ui/page-size))))

  (GET "/selected" []
       (html/html (ui/build-selected (get-user-id))))

  (GET "/read" [id url]
       (let [[feed item-id] (parse-item-id id)
             result (ui/build-content (get-user-id) item-id feed url)]
         (if (string? result)
           {:status 302 :headers {"Location" result}}
           (html/html result))))

  (POST "/next" {{np "next-position"
                  si "selected-item"} :form-params}
        (let [positions (map parse-item-id (ensure-coll np))
              selected-ids (map parse-item-id (ensure-coll si))]
          (ui/mark-read (get-user-id) positions selected-ids)
          {:status 302 :headers {"Location" "/"}}))

  (POST "/archive" {{items "selected-item"} :form-params}
        (ui/archive-items (get-user-id)
                          (map as-int (ensure-coll items)))
        {:status 302 :headers {"Location" "selected"}})

  (GET "/settings" []
       (html/html (ui/build-settings (get-user-id))))

  (POST "/save-settings" {{feeds "feeds"} :form-params}
        (ui/save-settings (get-user-id) feeds)
        {:status 302 :headers {"Location" "/"}})

  (route/resources "/")

  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes (assoc-in site-defaults [:security :anti-forgery] false)))

; === Debugging convenience functions ===

(defn _drop-cache []
  (reset! feed/dir-cache {})
  (memz/memo-clear! feed/get-data))

(use 'ring.adapter.jetty)

(defn _run-srv []
  (reset! feed/feed-dir (feed/load-feed-dirs))
  (run-jetty app {:port 8080 :join? false}))
