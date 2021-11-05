(ns feedcircuit-revisited.ui
  (:require [clojure.string :as s]
            [feedcircuit-revisited.backend :as backend]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.auth :as auth]
            [clojure.tools.logging :as log]))

(def page-size 16)

(defn get-next-positions [user-items]
  (into {} (map #(vector (:feed %) (inc (:num %))) user-items)))

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
    (for [[idx {title :title
                summary :summary
                content :content
                uid :uid
                feed :feed
                feed-title :feed-title}] (map-indexed vector items)]
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
                         {:href (str "feed?url=" feed)}
                         feed-title]
                        " "
                        [:label {:class "item-check"
                                 :for (ch-id idx)} icon]
                        ))))))

(defn build-feed [user-id feed from item-count extra-style]
  (let [item-count (or item-count page-size)
        items (take item-count (backend/get-feed-items feed from))
        next-from (dec (or (:num (last items)) 0))
        title (:title (backend/get-feed-attrs feed))
        checked (backend/get-selected-for-feed (backend/get-user-attrs user-id) feed)]
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
  (let [{sources :sources
         selected :selected} (backend/get-user-data user-id)
        items (take item-count (backend/get-unread-items sources))
        next-positions (get-next-positions items)
        checked (set (map :uid selected))]
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
  (let [link (:link item)]
    (if (coll? link) (first link) link)))

(defn find-styles [user-id url]
  (let [styles (:styles (backend/get-user-attrs user-id))]
    (map second (filter (fn [[pattern style]]
                          (s/includes? url pattern)) styles))))

(defn build-content [uid show-done extra-style user-id]
  (let [item (or (backend/get-item uid)
                 {:link uid})
        link (get-item-link item)
        content-ident (when-let [feed (:feed item)]
                        (:content-ident (backend/get-feed-attrs feed)))
        site-styles (find-styles user-id link)]
    (or
     (try
       (let [{title :title
              content :content
              author :author
              category :category
              comments :comments
              uid :uid} (content/augment item content-ident)]
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
    (backend/update-user-attrs! user-id update :positions merge pos-map)))

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
     (let [sources (:sources (backend/get-user-data user-id))
           feed-urls (map :feed (filter :active sources))
           feeds (map backend/get-feed-attrs feed-urls)]
       (for [feed feeds]
         [:div.fcr-news-item 
          [:a.fcr-link {:href (str "feed?url=" (:url feed))}
           [:h1 (:title feed)]]
          (if-let [image-url (not-empty (:image feed))]
            [:img.fcr-feed-logo {:src image-url}]
            (feed-logo))
          (if-let [summary (not-empty (:summary feed))]
            [:p summary])
          [:p.fcr-item-footer
           [:a.fcr-link {:href (:url feed)} (:url feed)]
           (if-let [last-sync (:last-sync feed)]
             [:span ", updated&nbsp;at&nbsp;"
              [:script (format "document.write(new Date(\"%s\").toLocaleString());" last-sync)]
              [:noscript last-sync]])]]))]]])

(defn serialize-source [subj]
  (str (when-not (:active subj) "#")
       (:feed subj)
       (when (:filters subj) " ")
       (:filters subj)))

(defn serialize-style [subj]
  (s/join " " subj))

(defn build-settings [user-id extra-style]
  (let [{sources :sources
         styles :styles} (backend/get-user-data user-id)]
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
     {:active active
      :feed (if active feed (subs feed 1))}
     (when (not-empty filters)
       {:filters filters}))))

(defn parse-style [subj]
  (vec (s/split subj #"\s+" 2)))

(defn save-settings [user-id feed-lines style-lines]
  (let [sources (map parse-source (s/split-lines feed-lines))
        styles (map parse-style (s/split-lines style-lines))
        known-feeds (backend/all-feeds)
        new-feeds (->> sources
                       (filter :active)
                       (map :feed)
                       (remove known-feeds))]
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
