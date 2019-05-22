(ns feedcircuit-revisited.auth
  (:require [feedcircuit-revisited.conf :as conf]
            [clj-http.client :as http]
            [clojure.string :as cstr]
            [cheshire.core :as json]))

(defn get-provider-url [provider hint state]
  (let [base-url (get-in (conf/param :oauth provider :auth-endpoint))]
    (str base-url "&hint=" hint "&state=" state)))

(defn get-providers []
  (for [{title :title
         icon :icon
         url :auth-endpoint} (vals (conf/param :oauth))]
    {:title title
     :icon icon
     :url url}))

(defn obtain-tokens [url params auth-code]
  (let [reply (http/post url
                         {:as :json
                          :form-params (assoc params
                                              :grant_type "authorization_code"
                                              :code auth-code)})]
    (select-keys (:body reply) [:access_token :id_token])))

(defn obtain-email-from-api [url auth-header-type email-path access-token]
  (let [reply (http/get url
                        {:headers {"Authorization" (str auth-header-type " " access-token)}
                         :as :json})]
    (get-in reply (concat [:body] email-path))))

(defn from-utf8 [subj]
  (new java.lang.String subj "utf8"))

(defn from-base64 [subj]
  (.decode (java.util.Base64/getDecoder) subj))

(defn obtain-email-from-id-token [token]
  (-> (second (cstr/split token #"\."))
      from-base64
      from-utf8
      (json/parse-string true)
      :email))

(defn get-email [via code]
  (when-let [auth-conf (conf/param :oauth via)]
    (let [{tok-url :url
           tok-params :params} (:token-endpoint auth-conf)]
      (let [{access-token :access_token
             id-token :id_token} (obtain-tokens tok-url tok-params code)]
        (cond
          id-token (obtain-email-from-id-token id-token)
          access-token (let [{api-url :url
                              auth-header-type :auth-header-type
                              email-path :email-path} (:api-endpoint auth-conf)]
                         (obtain-email-from-api api-url
                                                auth-header-type
                                                email-path
                                                access-token)))))))
