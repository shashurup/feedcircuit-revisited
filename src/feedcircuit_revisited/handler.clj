(ns feedcircuit-revisited.handler
  (:require [clojure.xml :as xml]
            [clojure.set :as cset]
            [clojure.string :as cstr]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clj-http.client :as http-client]
            [java-time :as jt]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clj-template.html :as h]
            [clojure.core.memoize :as memz]))

(defn parse-int [s] (Integer. s))

(def config {:root-dir "./fc-data"})
(def block-size 100)

; === item storage ===

(defn store-file [filename data]
  (let [tempfilename (str filename ".temp")]
    (do
      (with-open [w (io/writer tempfilename)]
        (binding [*out* w] (pr data)))
      (fs/rename tempfilename filename))))

(defn load-file-or-nil [filename]
  (if (fs/exists? filename)
    (load-file filename)))

(def get-data (memz/memo load-file-or-nil))

(defn set-data [filename data]
  (do
    (store-file filename data)
    (memz/memo-clear! get-data [filename])))

(defn get-block [dir block-num]
  (get-data (str dir "/" block-num)))

(defn set-block [dir block-num data]
  (set-data (str dir "/" block-num) data))

(defn set-attrs [dir attrs]
  (set-data (str dir "/attrs") attrs))

(defn get-attrs [dir]
  (get-data (str dir "/attrs")))

(def dir-cache (atom {}))

(defn load-dir [dir]
  (let [block-list (->> (fs/list-dir dir)
                        (map fs/base-name)
                        (filter #(re-matches #"[0-9]+" %))
                        (map parse-int)
                        (sort))]
    {:last-block (last block-list)
     :known-ids (->> block-list
                     (map #(load-file (str dir "/" %))) ; avoid caching all blocks
                     (apply concat)
                     (map :id)
                     (set))}))

(defn get-dir-cache [dir key]
  (let [entry (or (get @dir-cache dir)
                  (let [entry (load-dir dir)]
                    (do
                      (swap! dir-cache assoc dir entry)
                      entry)))]
    (get entry key)))

(defn get-known-ids [dir]
  (get-dir-cache dir :known-ids))

(defn get-last-block-num [dir]
  (or (get-dir-cache dir :last-block) 0))

(defn get-items [dir start]
  (let [last-block-num (get-last-block-num dir)
        start-block (quot start block-size)
        start-offset (rem start block-size)
        items (apply concat (map #(get-block dir %)
                                 (range start-block (inc last-block-num))))]
    (nthrest items start-offset)))

(defn append-items! [dir items]
  (let [last-block-num (get-last-block-num dir)
        known-ids (get-known-ids dir)
        last-block (get-block dir last-block-num)
        new-blocks (->> (concat last-block items)
                        (partition-all block-size)
                        (map vec))]
    (do
      (doseq [[num block] (map-indexed vector new-blocks)]
        (set-block dir (+ last-block-num num) block))
      (swap! dir-cache assoc dir {:last-block (+ last-block-num
                                                 (dec (count new-blocks)))
                                  :known-ids (cset/union known-ids
                                                         (set (map #(:id %) items)))}))))

; === feed parsing ===

(def attr-map {:guid :id
               :pubDate :published
               :updated :published
               :description :summary})

(def array-attrs #{:author :category :contributor :link})

(defn content-str [attr] (apply str (:content attr)))

(defn get-link-url [attr] (or (get-in attr [:attrs :href])
                              (content-str attr)))

(defn from-rfc1123-datetime [attr] (str (jt/instant (jt/formatter :rfc-1123-date-time)
                                                    (content-str attr))))

(def attr-convert {:pubDate from-rfc1123-datetime
                   :link get-link-url})

(defn parse-rss-item-attribute [item attr]
  (let [tag (:tag attr)
        conv (get attr-convert tag content-str)
        upd-fn (if (contains? array-attrs tag) (fn [old new] (conj (or old []) new))
                                               (fn [old new] new))]
    (update-in item
               [(get attr-map tag tag)]
               upd-fn (conv attr))))

(defn parse-rss-item [item]
  (reduce parse-rss-item-attribute {} (:content item)))

(defn find-root [feed-xml]
  (or (first (filter #(= (:tag %) :channel)
                     (:content feed-xml)))
             feed-xml))

(defn extract-rss-items [feed-xml]
  (let [root (find-root feed-xml)]
    (filter #(contains? #{:item :entry} (:tag %)) (:content root))))

(defn parse-feed-details [feed-xml]
  (->> (:content (find-root feed-xml))
       (filter #(not (contains? #{:item :entry} (:tag %))))
       (reduce parse-rss-item-attribute {})))

(defn fetch-new-items [url known-ids]
  (let [feed-xml (xml/parse url)
        attrs (parse-feed-details feed-xml)
        items (map parse-rss-item (extract-rss-items feed-xml))
        new-items (->> items
                       (filter #(not (contains? known-ids (:id %))))
                       (sort-by :published))]
     [attrs new-items]))

; === feed handling ===

(defn load-feed-dirs []
  (->> (fs/list-dir (str (:root-dir config) "/feeds"))
       (map fs/normalized)
       (filter fs/directory?)
       (map #(vector (:url (get-attrs %)) (str %)))
       (into {})))

(def feed-dir (atom (load-feed-dirs)))

(defn dir-name [url]
  (-> url
      (cstr/replace "http://" "")
      (cstr/replace "https://" "")
      (cstr/replace "/" ".")))

(defn dir-path [url]
  (str (fs/normalized (str (:root-dir config) "/feeds/" (dir-name url)))))

(defn sync-feed! [url]
  (let [dir (get @feed-dir url (dir-path url))
        known-ids (get-known-ids dir)
        [attrs new-items] (fetch-new-items url known-ids)]
    (do
      (fs/mkdirs dir)
      (set-attrs dir (assoc attrs :url url))
      (append-items! dir new-items)
      (swap! feed-dir assoc url dir))))

(defn get-feed-items [url start]
  (map-indexed #(assoc %2 :num (+ start %1))
               (get-items (get @feed-dir url) start)))

; === user handling ===

(def page-size 16)

(defn get-user-items [user count]
  (let [{feeds :feeds
         positions :positions} user]
    (->> feeds
         (map #(vector % (get-feed-items % (get positions % 0))))
         (mapcat (fn [[feed items]]
                   (map #(assoc % :feed feed) items)))
         (take count))))

(defn user-dir [id]
  (str (:root-dir config) "/users/" id))

(defn get-user-attrs [id]
  (assoc (get-attrs (user-dir id))
         :id id))

(defn update-user-attrs! [attrs]
  (let [dir (user-dir (:id attrs))]
    (fs/mkdirs dir)
    (set-attrs dir (dissoc attrs :id))))

(defn select-items! [user ids]
  (let [dir (user-dir user)
        items (map #(first (apply get-feed-items %)) ids)]
    (append-items! dir items)))

; === web interface ===

(defn get-next-positions [user-items]
  (into {} (map #(vector (:feed %) (inc (:num %))) user-items)))

(defn lines [coll]
  (apply str (interpose "\n" coll)))

(defn ensure-coll [x]
  (cond
    (coll? x) x
    x [x]))

(defn parse-item-id [item-id]
  (let [[_ ord-num feed] (re-matches #"([0-9]+),(.*)" item-id)]
    [feed (parse-int ord-num)]))

(defroutes app-routes
  (GET "/" {{count-param :count} :params}
       (let [item-count (if count-param (parse-int count-param) page-size)
             user (get-user-attrs "georgy@kibardin.name")
             items (get-user-items user item-count)
             next-positions (get-next-positions items)
             items-html (for [{title :title
                               summary :summary
                               link :link
                               feed :feed
                               ord-num :num} items]
                          (h/div {:class "news-item"} "\n"
                                 (h/input {:type "checkbox"
                                           :name "selected-item"
                                           :value (str ord-num "," feed)})
                                 (h/a {:href (first link) :class "news-header"} title) (h/br-) "\n"
                                 summary))
             inputs-html (for [[feed pos] next-positions]
                           (h/input {:type "hidden"
                                     :name "next-position"
                                     :value (str pos "," feed)}))]
         (h/html "\n"
          (h/head "\n"
           (h/title "Welcome to Feedcircuit") "\n"
           (h/link {:rel "stylesheet" :type "text/css" :href "/style.css"})) "\n"
          (h/body "\n"
           (h/form {:action "/markread" :method "POST"} "\n"
                   (h/div {:class "news-list"}
                          (lines items-html)) "\n"
                   (lines inputs-html)
                  (h/input {:type "submit" :value "Next"}))))))
  (POST "/markread" {params :form-params}
        (let [user (get-user-attrs "georgy@kibardin.name")
              pos-params (ensure-coll (get params "next-position"))
              positions (into {} (map parse-item-id pos-params))
              selected-items (map parse-item-id (ensure-coll (get params "selected-item")))]
          (do
            (update-user-attrs! (update-in user [:positions] merge positions))
            (select-items! (:id user) selected-items)
            {:status 302 :headers {"Location" "/"}})))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes (assoc-in site-defaults [:security :anti-forgery] false)))

; (use 'ring.adapter.jetty)
; (def ring-server (run-jetty app {:port 8080 :join? false}))
