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
            [hiccup.core :as html]
            [feedcircuit-revisited.content :as content]
            [clojure.core.memoize :as memz]))

(defn parse-int [s] (if s (Integer. s)))

(def config {:root-dir "./fc-data"})
(def block-size 100)

; === item storage ===

(defn write-file [filename data]
  (let [tempfilename (str filename ".temp")]
    (with-open [w (io/writer tempfilename)]
      (binding [*out* w] (pr data)))
    (fs/rename tempfilename filename)))

(defn read-file [filename]
  (if (fs/exists? filename)
    (with-open [r (java.io.PushbackReader. (io/reader filename))]
      (read r))))

(def get-data (memz/memo read-file))

(defn set-data [filename data]
  (write-file filename data)
  (memz/memo-clear! get-data [filename]))

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
                        sort
                        vec)]
    {:item-count (if (empty? block-list)
                   0
                   (+ (* (dec (count block-list)) block-size)
                      (count (get-block dir (last block-list)))))
     :known-ids (->> block-list
                     (map #(read-file (str dir "/" %))) ; avoid caching all blocks
                     (apply concat)
                     (map :id)
                     (set))}))

(defn get-dir-cache [dir key]
  (let [entry (or (get @dir-cache dir)
                  (let [entry (load-dir dir)]
                    (swap! dir-cache assoc dir entry)
                    entry))]
    (get entry key)))

(defn get-known-ids [dir]
  (get-dir-cache dir :known-ids))

(defn get-item-count [dir]
  (or (get-dir-cache dir :item-count) 0))

(defn get-last-block-num [dir]
  (quot (get-item-count dir) block-size))

(defn get-items [dir start]
  (let [last-block-num (get-last-block-num dir)
        start-block (quot start block-size)
        start-offset (rem start block-size)
        items (apply concat (map #(get-block dir %)
                                 (range start-block (inc last-block-num))))]
    (nthrest items start-offset)))

(defn get-numbered-items [dir start]
  (map-indexed #(assoc %2 :num (+ start %1))
               (get-items dir start)))

(defn append-items! [dir items]
  (let [last-block-num (get-last-block-num dir)
        last-block (get-block dir last-block-num)
        new-blocks (->> (concat last-block items)
                        (partition-all block-size)
                        (map vec))
        known-ids (get-known-ids dir)
        start (get-item-count dir)]
    (doseq [[num block] (map-indexed vector new-blocks)]
      (set-block dir (+ last-block-num num) block))
    (swap! dir-cache assoc dir {:item-count (+ start (count items))
                                :known-ids (cset/union known-ids
                                                       (set (map #(:id %) items)))})
    (range start (+ start (count items)))))

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

(defn average [coll]
  (quot (apply + coll) (count coll)))

(defn long-content? [items]
  (->> items
       (map :summary)
       (map count)
       average
       (<= 1024)))

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

(defn extract-summary [item]
  (let [content (:summary item)
        summary (content/summarize content)]
    (if summary
      (assoc item :content content
             :summary summary)
      item)))

(defn preproces [items attrs]
  (if (:long-content attrs)
    (map extract-summary items)
    items))

(defn add-feed! [url]
  (let [dir (dir-path url)
        [attrs new-items] (fetch-new-items url #{})]
    (fs/mkdirs dir)
    (swap! feed-dir assoc url dir)
    (set-attrs dir (assoc attrs
                          :url url
                          :long-content (long-content? new-items)))
    (append-items! dir (preproces new-items
                                  (get-attrs dir)))))

(defn sync-feed! [url]
  (let [dir (get @feed-dir url)
        known-ids (get-known-ids dir)
        [new-attrs new-items] (fetch-new-items url known-ids)]
    (set-attrs dir (merge (get-attrs dir)
                          new-attrs))
    (append-items! dir (preproces new-items
                                  (get-attrs dir)))))

(defn next-update-time [url]
  (let [dir (get @feed-dir url)
        last-items (get-items dir (max 0 (- (get-item-count dir) 10)))
        dates (->> last-items
                   (map :published)
                   (map #(jt/instant (jt/formatter :iso-date-time) %)))
        deltas (->> dates
                    (map jt/to-millis-from-epoch)
                    (partition 2 1)
                    (map (fn [[b e]] (- e b))))]
    (if (empty? deltas)
      (jt/instant)
      (jt/plus (apply jt/max dates)
               (jt/min (jt/hours 24)
                       (jt/millis (quot (apply + deltas)
                                        (count deltas))))))))

(defn sync! []
  (->> (keys @feed-dir)
       (filter #(jt/before? (next-update-time %)
                            (jt/instant)))
       (map sync-feed!)))

; === user handling ===

(def page-size 16)

(defn get-user-items [user count]
  (let [{feeds :feeds
         positions :positions} user]
    (->> feeds
         (map #(vector % (get-numbered-items (get @feed-dir %)
                                             (get positions % 0))))
         (mapcat (fn [[feed items]]
                   (map #(assoc % :feed feed) items)))
         (take count))))

(defn user-dir [id]
  (str (:root-dir config) "/users/" id))

(defn get-user-attrs [id]
  (-> (get-attrs (user-dir id))
      (assoc :id id)
      (update :unread #(apply sorted-set %))))

(defn update-user-attrs! [attrs]
  (let [dir (user-dir (:id attrs))]
    (fs/mkdirs dir)
    (set-attrs dir
               (-> attrs
                   (dissoc :id)
                   (update :unread vec)))))

(defn select-items! [user ids]
  (let [dir (user-dir user)
        items (map (fn [[url pos]]
                     (first (get-items (get @feed-dir url) pos))) ids)]
    (fs/mkdirs dir)
    (append-items! dir items)))

(defn get-selected-items [user-id]
  (let [dir (user-dir user-id)
        user (get-user-attrs user-id)]
    (map #(first (get-numbered-items dir %))
         (:unread user))))

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

(defn bookmark-icon-svg []
  [:svg {:height  "auto"
         :viewBox "-3 -3 66 99"}
   [:polygon {:class  "bookmark-icon"
              :points "0,0 0,90 30,60 60,90 60,0"
              :style  "stroke-width:6"}]])

(defn checkbox-svg []
  [:svg {:height  "auto"
         :viewBox "0 0 60 60"}
   [:rect {:width  "60"
           :height "60"
           :style  "stroke-width:3;fill:none"}]
   [:polyline {:class  "checkmark"
               :points "10,20 20,40 50,25"
               :style  "fill:none;stroke-width:8"}]])

(defn svg-checkbox [input-attrs svg]
  [:label {:class "svg-checkbox"}
   [:input (merge {:type "checkbox"}
                  input-attrs)]
   svg])

(defn news-item [url title summary mark]
  [:div {:class "news-item"}
   [:div {:class "news-header"}
    [:a {:href url
         :target "_blank"} title]]
   summary "&nbsp;" mark])

(defn build-feed [user-id item-count]
  (let [user (get-user-attrs user-id)
        items (get-user-items user item-count)
        next-positions (get-next-positions items)]
    [:html
     [:head
      [:title "Feedcircuit"]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
      [:link {:rel "stylesheet" :type "text/css" :href "style.css"}]]
     [:body
      [:form {:action "next" :method "POST"}
       [:div {:class "news-list"}
        (for [{title :title
               summary :summary
               content :content
               link :link
               feed :feed
               ord-num :num} items]
          (news-item (str "item?id=" ord-num "," feed)
                     title
                     (or summary content)
                     (svg-checkbox {:name "selected-item"
                                    :value (str ord-num "," feed)}
                                   (bookmark-icon-svg))))
        (if (empty? items)
          [:p.no-more "No more items"])
        (for [[feed pos] next-positions]
          [:input {:type "hidden"
                   :name "next-position"
                   :value (str pos "," feed)}])
        (if-not (empty? items)
          [:input {:class "nav-btn"
                   :type "submit"
                   :value (str "Next " page-size " >>")}])
        [:a {:class "nav-btn nav-btn-right"
             :href "selected"} "Go to selected items"]]]]]))

(defn build-selected [user-id]
  (let [items (get-selected-items user-id)]
    [:html
     [:head
      [:title "Feedcircuit, selected items"]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
      [:link {:rel "stylesheet" :type "text/css" :href "style.css"}]
      [:script {:src "code.js"}]]
     [:body
      [:div {:class "news-list"}
       (for [{title :title
              summary :summary
              content :content
              link :link
              ord-num :num} items]
         (news-item (str "item?id=" ord-num ",")
                    title
                    (or summary content)
                    (svg-checkbox {:value ord-num
                                   :onchange "setItemState(this.value, this.checked);" }
                                  (checkbox-svg))))
       (if (empty? items)
         [:p.no-more "No more items"])
       [:a {:class "nav-btn nav-btn-right"
            :href "./"} "Back to the feed"]]]]))

(defn build-content [user-id item-id feed]
  (let [dir (if (empty? feed) (user-dir user-id) (get @feed-dir feed))
        item (first (get-items dir item-id))
        content (:content item)
        link (first (:link item))
        title (:title item)]
    (if content
      [:html
       [:head
        [:title title]
        [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
        [:link {:rel "stylesheet" :type "text/css" :href "/style.css"}]]
       [:body
        [:div {:class "news-list"}
         (news-item link
                    title
                    content
                    "")]]]
      link)))

(defn mark-read [user-id to-positions selected]
  (let [user (get-user-attrs user-id)]
    (-> (update-in user [:unread] into (select-items! user-id selected))
        (update-in [:positions] merge (into {} to-positions))
        (update-user-attrs!))))

(defn mark-item [user-id item-id state]
  (let [user (get-user-attrs user-id)]
    (update-user-attrs!
     (update user :unread (if state disj conj) item-id))))

(defn get-user-id [] "georgy@kibardin.name")

(defroutes app-routes
  (GET "/" [count]
       (html/html (build-feed (get-user-id)
                              (or (parse-int count) page-size))))

  (GET "/selected" []
       (html/html (build-selected (get-user-id))))

  (GET "/item" [id :<< parse-item-id]
       (let [[feed item-id] id
             result (build-content (get-user-id) item-id feed)]
         (if (string? result)
           {:status 302 :headers {"Location" result}}
           (html/html result))))

  (POST "/next" {params :form-params}
        (let [positions (map parse-item-id (ensure-coll (get params "next-position")))
              selected-ids (map parse-item-id (ensure-coll (get params "selected-item")))]
          (mark-read (get-user-id) positions selected-ids)
          {:status 302 :headers {"Location" "/"}}))

  (POST "/mark-item" [item-id :<< parse-int
                      read :<< #(= % "true")]
        (mark-item (get-user-id) item-id read)
        {:status 200})

  (route/resources "/")

  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes (assoc-in site-defaults [:security :anti-forgery] false)))

; === Debugging convenience functions ===

(defn _drop-cache []
  (reset! dir-cache {})
  (memz/memo-clear! get-data))

(use 'ring.adapter.jetty)

(defn _run-srv []
  (reset! feed-dir (load-feed-dirs))
  (run-jetty app {:port 8080 :join? false}))
