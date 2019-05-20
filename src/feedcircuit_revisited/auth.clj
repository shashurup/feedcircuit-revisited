(ns feedcircuit-revisited.auth
  (:require [feedcircuit-revisited.conf :as conf]
            [clj-http.client :as http]))

(defn get-providers []
  (for [{title :title
         icon :icon
         url :auth-endpoint} (vals (conf/param :oauth))]
    {:title title
     :icon icon
     :url url}))

(defn get-email [via code]
  (when-let [auth-conf (conf/param :oauth via)]
    (let [{tok-url :url
           tok-params :params} (:token-endpoint auth-conf)
          reply (http/post tok-url
                           {:as :json
                            :form-params (assoc tok-params
                                                :grant_type "authorization_code"
                                                :code code)})]
      (if-let [access-token (get-in reply [:body :access_token])]
        (let [{api-url :url
               auth-header-type :auth-header-type
               email-path :email-path} (:api-endpoint auth-conf)
              reply (http/get api-url
                              {:headers {"Authorization" (str auth-header-type " " access-token)}
                               :as :json})]
          (get-in reply (concat [:body] email-path)))))))
