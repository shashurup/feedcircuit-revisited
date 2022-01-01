(ns feedcircuit-revisited.ui
  (:require [clojure.string :as s]
            [feedcircuit-revisited.backend :as backend]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.auth :as auth]
            [feedcircuit-revisited.utils :as u]
            [clojure.tools.logging :as log]))

(def page-size 16)

(defn get-next-positions [user-items]
  (into {} (map #(vector (:item/source %) (inc (:item/num %))) user-items)))

(defn bookmark-icon-svg []
  [:svg {:viewBox "-3 -3 66 99"}
   [:polygon {:class  "bookmark-icon"
              :points "0,0 0,90 30,60 60,90 60,0"
              :style  "stroke-width:6"}]])

(defn backspace-svg []
  [:svg {:viewBox "-2 -2 33 33"}
   [:polygon {:points "10,0 0,10 10,20 30,20 30,0"
              :style "stroke-width:2;fill:none;stroke-linejoin:round"}]
   [:line {:x1 "15" :y1 "5" :x2 "25" :y2 "15"
           :style "stroke-width:2;stroke-linecap:round"}]
   [:line {:x1 "15" :y1 "15" :x2 "25" :y2 "5"
           :style "stroke-width:2;stroke-linecap:round"}]])

(defn svg-checkbox [input-attrs svg]
  [:label {:class "svg-checkbox"}
   [:input (merge {:type "checkbox"}
                   input-attrs)]
   svg])

(defn ch-id [idx] (str "ch" idx))

(defn item-checkbox [idx class onchange value checked]
  (let [attrs {:type "checkbox"
               :id (ch-id idx)
               :class (str "item-check " class)
               :name "selected-item"
               :form "main"
               :onchange (or onchange "")
               :value value}]
    [:input (merge attrs (when checked {:checked "yes"}))]))

(defn news-item [url title summary footer]
  [:div.fcr-news-item
   [:div.fcr-news-header
    [:a.fcr-link {:href url
                  :rel "opener"} [:h1 title]]]
   [:div.fcr-news-body summary footer]])

(defn news-content [url title content footer]
  [:div.fcr-article
   [:a.fcr-link {:href url} [:h1 title]]
   content
   [:div.fcr-item-footer footer]])

(defn get-url-path [url]
  (.getPath (new java.net.URL (new java.net.URL "file:") url)))

(defn style-or-script [url]
  (if (s/ends-with? (get-url-path url)
                    ".js")
    [:script {:src url}]
    [:link {:rel "stylesheet" :type "text/css" :href url}]))

(defn head [title & styles]
  [:head
   [:title title]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
   [:script {:src "code.js"}]
   [:link {:rel "stylesheet" :type "text/css" :href "style.css"}]
   (for [style styles :when (seq style)]
     (style-or-script style))
   [:link {:rel "shortcut icon" :type "image/png" :href "favicon.png"}]])

(defn submit-button
  ([caption] (submit-button caption false))
  ([caption disabled]
   [:input (merge {:class (str "fcr-btn"
                               (if disabled " disabled" ""))
                   :type "submit"
                   :value caption}
                  (if disabled {:disabled true} {}))]))

(defn nav-entry [entry href selected]
  (let [title (s/capitalize (s/replace-first (str entry) ":" ""))]
    (if (= entry selected)
      [:span title]
      [:a {:href href} title])))

(defn navbar [user-id selected]
  [:div.fcr-nav-bar
   (nav-entry :feed "./" selected) " | "
   (nav-entry :selected "selected" selected) " | "
   [:div.fcr-menu
    [:a {:href "/extra-links"} "..."]
    [:div.fcr-menu-items
     [:div.fcr-menu-item "Logged in as:" [:br] user-id]
     [:div.fcr-menu-item (nav-entry :sources "sources" selected)]
     [:div.fcr-menu-item (nav-entry :settings "settings" selected)]
     [:div.fcr-menu-item [:a {:href "/logout"} "Logout"]]]]])

(defn build-extra-links [user-id extra-style]
  [:html
   (head "Feedcircuit" extra-style)
   [:body
    [:p "Logged in as " user-id]
    [:p [:a {:href "/sources"} "Sources"]]
    [:p [:a {:href "/settings"} "Settings"]]
    [:p [:a {:href "/logout"} "Logout"]]]])

(defn build-item-list [items class icon source checked]
  (if (empty? items)
    [:p.fcr-no-more-items "No more items"]
    (for [[idx {link :item/link
                title :item/title
                summary :item/summary
                content :item/content
                uid :item/id
                feed :item/feed
                feed-title :feed/title}] (map-indexed vector items)]
      (list (item-checkbox idx
                           class
                           "toggleItem(this);"
                           uid
                           (checked uid))
            (news-item (str "plain?source=" source "&url=" uid)
                       title
                       (or summary content)
                       (list
                        " |&nbsp;"
                        [:a.fcr-link.fcr-item-footer
                         {:href (if feed
                                  (str "feed?url=" feed)
                                  (u/get-url-base link))}
                         (or feed-title (u/get-url-host link))]
                        "&nbsp;~ "
                        [:label {:class "item-check"
                                 :for (ch-id idx)} icon]
                        ))))))

(defn build-feed [user-id feed from item-count extra-style]
  (let [item-count (or item-count page-size)
        items (take item-count (backend/get-feed-items feed from))
        next-from (dec (or (:item/num (last items)) 0))
        title (backend/get-feed-attr-by-id feed :feed/title)
        checked (set (map :item/id (:user/selected (backend/get-user-data user-id))))]
    [:html
     (head title extra-style)
     [:body
      (navbar user-id :a-feed)
      [:div.fcr-wrapper.fcr-ui
       (build-item-list items
                        "fill-checked"
                        (bookmark-icon-svg)
                        "fc"
                        checked)
       (if (>= next-from 0)
         [:a.fcr-btn
          {:href (str "feed?from=" next-from "&count=" item-count "&url=" feed)}
          (str "<< Previous " item-count)])]]]))

(defn build-unread [user-id item-count extra-style]
  (let [{sources :user/sources
         selected :user/selected} (backend/get-user-data user-id :sources/feed-title)
        items (take item-count (backend/get-unread-items sources))
        next-positions (get-next-positions items)
        checked (set (map :item/id selected))]
    [:html
     (head "Feedcircuit" extra-style)
     [:body
      (navbar user-id :feed)
      [:div.fcr-wrapper.fcr-ui
       (build-item-list items
                        "fill-checked"
                        (bookmark-icon-svg)
                        "fc"
                        checked)
       [:form {:id "main" :action "next" :method "POST"}
        (for [[feed pos] next-positions]
          [:input {:type "hidden"
                   :name "next-position"
                   :value (str pos "," feed)}])
        (if-not (empty? items)
          (submit-button (str "Next " page-size " >>")))]]]]))

(defn build-selected [user-id extra-style]
  (let [items (backend/get-selected-items user-id)]
    [:html
     (head "Feedcircuit, selected items" extra-style)
     [:body
      (navbar user-id :selected)
      [:div.fcr-wrapper.fcr-ui
       (build-item-list items
                        "gray-checked selected-item"
                        (backspace-svg)
                        "selected"
                        #{})
       [:noscript
        [:form {:id "main" :action "complete-selected" :method "POST"}
         (submit-button "Done")]]]]]))

(defn get-item-link [item]
  (let [link (:item/link item)]
    (if (coll? link) (first link) link)))

(defn find-styles [user-id url]
  (let [styles (:user/styles (backend/get-user-data user-id))]
    (map second (filter (fn [[pattern style]]
                          (s/includes? url pattern)) styles))))

(defn retrieve-content [url feed]
  (let [html (content/retrieve-and-parse url)
        content-ident (backend/get-feed-attr-by-id feed :feed/content-ident)]
    (content/detect html url content-ident)))

(defn ensure-content [item]
  (if (:item/content item)
    item
    (if-let [{url :item/link feed :item/feed} item]
      (assoc item :item/content (retrieve-content url feed)))))

(defn make-item-from [url]
  (let [html (content/retrieve-and-parse url)
        content (content/detect html url nil)]
    {:item/link url
     :item/id url
     :item/title (content/get-title html)
     :item/summary (content/summarize (vec (conj content :body)))
     :item/content content}))

(defn build-content [uid show-done extra-style user-id]
  (let [item (or (backend/get-item uid)
                 (make-item-from uid))
        link (get-item-link item)
        site-styles (find-styles user-id link)]
    (or
     (try
       (let [{title :item/title
              content :item/content
              author :item/author
              category :item/category
              comments :item/comments
              uid :item/id} (ensure-content item)]
         (if content
           [:html
            (apply head title extra-style site-styles)
            [:body
             [:div.fcr-wrapper
              (news-content link title content
                            (list
                             (if (not (empty? author))
                               [:p (str "Author: " (s/join ", " author))])
                             (if (not (empty? category))
                               [:p (str "Category: " (s/join ", " category))])
                             (if comments
                               [:p [:a {:href comments} "Comments"]])))
              (if show-done
                [:form {:action "complete-selected" :method "POST"}
                 [:input {:type "hidden" :name "selected-item" :value uid}]
                 (submit-button "Done")])]]]))
       (catch Exception ex
         (log/error "Failed to make content for" link)))
     link)))

(defn mark-read [user-id to-positions]
  (let [pos-map (into {} to-positions)]
    (backend/update-positions! user-id pos-map)))

(defn queue-content-caching [uid]
  (future
    (if-let [{url :item/link feed :item/feed} (backend/get-item uid)]
      (let [content (content/detect (content/retrieve-and-parse url)
                                    url
                                    (backend/get-feed-attr-by-id feed :feed/content-ident))]
        (backend/add-content! uid content)))))

(defn selected-add! [user-id ids]
  (let [ids-to-cache (filter backend/item-id? ids)]
    (->> ids
         (map #(if (backend/item-id? %) % (make-item-from %)))
         (backend/selected-add! user-id))
    (map queue-content-caching ids-to-cache)))

(defn feed-logo []
  [:svg.fcr-feed-logo {:viewBox "0 0 50 50"
                       :fill    "none"}
   [:circle {:cx 10 :cy 40 :r 5 :style "stroke-width:6"}]
   [:path {:d "M 30 40 a 30 30 0 0 0 -20 -20"
           :style "stroke-width:8;stroke-linecap:round"}]
   [:path {:d "M 45 40 a 45 45 0 0 0 -35 -35"
           :style "stroke-width:8;stroke-linecap:round"}]])

(defn build-sources [user-id extra-style]
  [:html
   (head "Feedcircuit, news sources" extra-style)
   [:body
    (navbar user-id :sources)
    [:div.fcr-wrapper.fcr-ui
     (let [sources (->> (backend/get-user-data user-id :sources/feed-details)
                        :user/sources
                        (filter :source/active))]
       (for [source sources]
         [:div.fcr-news-item 
          [:a.fcr-link {:href (str "feed?url=" (:source/feed source))}
           [:h1 (:feed/title source)]]
          (if-let [image-url (not-empty (:feed/image source))]
            [:img.fcr-feed-logo {:src image-url}]
            (feed-logo))
          (if-let [summary (not-empty (:feed/summary source))]
            [:p summary])
          [:p.fcr-item-footer
           [:a.fcr-link {:href (:feed/url source)} (:feed/url source)]
           (if-let [last-sync (:feed/last-sync source)]
             [:span ", updated&nbsp;at&nbsp;"
              [:script (format "document.write(new Date(\"%s\").toLocaleString());" last-sync)]
              [:noscript last-sync]])]]))]]])

(defn serialize-source [subj]
  (str (when-not (:source/active subj) "#")
       (:feed/url subj)
       (when (:source/filters subj) " ")
       (:source/filters subj)))

(defn serialize-style [subj]
  (s/join " " subj))

(defn build-settings [user-id extra-style]
  (let [{sources :user/sources
         styles :user/styles} (backend/get-user-data user-id)]
    [:html
     (head "Feedcircuit settings" extra-style)
     [:body {:onLoad "initAppearance();"}
      (navbar user-id :settings)
      [:div.fcr-wrapper.fcr-ui
       [:p [:h1 "Sources"]]
       [:p
        "Each line in the list below defines a news source to constitute your feed. "
        "In the simplest case it is just an URL of RSS or Atom feed. "]
       [:p [:code "http://example.com/rss.xml"]]
       [:p "The more sofisticated setup allows you to filter a news source by its author or category. "
           "For instance"]
       [:p [:code "http://example.com/atom.xml Sport, Science, John Doe"]]
       [:p "selects only entries in Sport and Science categories and by John Doe. Finally,"]
       [:p [:code "http://example.com/rss.xml !Politics"]]
       [:p "selects everything except Politics category."]
       [:form {:action "settings" :method "POST"}
        [:textarea#feeds {:class "fcr-setting-input" :name "feeds"}
         (s/join "\n" (map serialize-source sources))]
        [:a.fcr-link {:onClick "toggleAppearance();" :href "#"}
         [:h1#appearance-header "Appearance settings"]]
        [:div#appearance {:style "display: none"}
         [:p "External stylesheet url makes it possible to customize Feedcircuit appearance. "
          "There are two builtin color schemes olive.css and blued-charcoal.css. "
          "The url is stored on per browser basis."]
         [:input.fcr-setting-input {:name "extra-style" :value extra-style}]
         [:p "Each source peculiarities can also be taken into account "
          "by setting site specific stylesheets."]
         [:p "Each line contains site and corresponing stylesheet."]
         [:p [:code "example.com  http://www.example.com/style.css"]]
         [:p "The site is expected to be a substring of the source url."]
         [:textarea#styles {:class "fcr-setting-input" :name "styles"}
          (s/join "\n" (map serialize-style styles))]]
        [:div.fcr-bottom-buttons
         (submit-button "Save")]]]]]))

(defn parse-source [subj]
  (let [active (not (s/starts-with? subj "#"))
        [feed filters] (s/split subj #"\s+" 2)]
    (merge 
     {:source/active active
      :source/feed (if active feed (subs feed 1))}
     (when (not-empty filters)
       {:source/filters filters}))))

(defn parse-style [subj]
  (vec (s/split subj #"\s+" 2)))

(defn save-settings [user-id feed-lines style-lines]
  (let [sources (map parse-source (s/split-lines feed-lines))
        styles (map parse-style (s/split-lines style-lines))
        new-feeds (->> sources
                       (filter :source/active)
                       (map :source/feed)
                       backend/unknown-feeds)]
    (doseq [url new-feeds]
      (feed/add-feed! url))
    (backend/update-settings! user-id sources styles)))

(defn build-login-options [extra-style]
  [:html
   (head "Welcome to Feedcircuit" extra-style)
   [:body
    [:div.hv-center
     [:div#fcr-login-options
      [:p "Sign in with:"]
      (for [{title :title
             icon :icon
             url :url} (auth/get-providers)]
        [:p [:a {:href url}
             (when icon
               [:img {:src icon}])
             title]])]]]])
