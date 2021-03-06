(ns feedcircuit-revisited.ui
  (:require [clojure.string :as s]
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
               :onchange (or onchange "")
               :value value}]
    [:input (merge attrs (when checked {:checked "yes"}))]))

(defn news-item [url title summary footer]
  [:div.fcr-news-item
   [:div.fcr-news-header
    [:a.fcr-link {:href url
                  :rel "opener"
                  :target "_blank"} [:h1 title]]]
   [:div.fcr-news-body summary footer]])

(defn news-content [url title content footer]
  [:div.fcr-article
   [:a.fcr-link {:href url
                 :target "_blank"} [:h1 title]]
   content
   [:div.fcr-item-footer footer]])

(defn head [title & styles]
  [:head
   [:title title]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
   [:script {:src "code.js"}]
   [:link {:rel "stylesheet" :type "text/css" :href "style.css"}]
   (for [style styles :when (seq style)]
     [:link {:rel "stylesheet" :type "text/css" :href style}])
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

(defn get-feed-title [iid]
  (if (coll? iid)
    (:title (feed/get-feed-attrs (first iid)))
    (.getHost (new java.net.URL iid))))

(defn iid-to-str [iid]
  (if (coll? iid)
    (str (second iid) "," (first iid))
    iid))

(defn build-item-list [items class icon source checked]
  (if (empty? items)
    [:p.fcr-no-more-items "No more items"]
    (for [[idx {title :title
                summary :summary
                content :content
                iid :iid}] (map-indexed vector items)]
      (list (item-checkbox idx
                           class
                           "toggleItem(this);"
                           (iid-to-str iid)
                           (checked iid))
            (news-item (str "plain?source=" source "&url=" (iid-to-str iid))
                       title
                       (or summary content)
                       (list
                        " |&nbsp;"
                        [:a.fcr-link.fcr-item-footer
                         {:href (str "feed?url=" (first iid))}
                         (get-feed-title iid)]
                        " "
                        [:label {:class "item-check"
                                 :for (ch-id idx)} icon]
                        ))))))

(defn build-feed [user-id feed from item-count extra-style]
  (let [total-count (feed/get-item-count feed)
        item-count (or item-count page-size)
        start-from (or from (if (> total-count item-count)
                              (- total-count item-count)
                              0))
        next-from (if (> start-from item-count)
                    (- start-from item-count)
                    0)
        next-count (if (> start-from item-count)
                     item-count
                     start-from)
        title (:title (feed/get-feed-attrs feed))
        items (reverse (take item-count (feed/get-feed-items feed start-from)))
        checked (feed/get-selected-for-feed (feed/get-user-attrs user-id) feed)]
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
       (if (> start-from 0)
         [:a.fcr-btn
          {:href (str "feed?from=" next-from "&count=" next-count "&url=" feed)}
          (str "<< Previous " next-count)])]]]))

(defn build-unread [user-id item-count extra-style]
  (let [user (feed/get-user-attrs user-id)
        items (take item-count (feed/get-unread-items user))
        next-positions (get-next-positions items)
        checked (feed/get-selected-among-unread user)]
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
       [:form {:action "/positions" :method "POST"}
        (for [[feed pos] next-positions]
          [:input {:type "hidden"
                   :name "next-position"
                   :value (str pos "," feed)}])
        (if-not (empty? items)
          (submit-button (str "Next " page-size " >>")))]]]]))

(defn build-selected [user-id extra-style]
  (let [items (feed/get-selected-items user-id)]
    [:html
     (head "Feedcircuit, selected items" extra-style)
     [:body
      (navbar user-id :selected)
      [:div.fcr-wrapper.fcr-ui
       (build-item-list items
                        "gray-checked selected-item"
                        (backspace-svg)
                        "selected"
                        #{})]]]))

(defn get-item-link [item]
  (let [link (:link item)]
    (if (coll? link) (first link) link)))

(defn find-style [user-id url]
  (let [styles (:styles (feed/get-user-attrs user-id))]
    (some (fn [[pattern style]]
            (if (s/includes? url pattern) style)) styles)))

(defn build-content [feed ord-num url source extra-style user-id]
  (let [item (if (not-empty feed)
               (first (feed/get-feed-items feed ord-num))
               {:link url})
        link (get-item-link item)
        content-ident (when (not-empty feed)
                        (:content-ident (feed/get-feed-attrs feed)))
        site-style (find-style user-id link)]
    (or
     (try
       (let [{title :title
              content :content
              author :author
              category :category
              comments :comments
              iid :iid} (content/augment item content-ident)
             iid (or iid url)
             done-action (if (= source "selected")
                           (str "UnselectAndClose('" (iid-to-str iid) "');")
                           "window.close();")]
         (if content
           [:html
            (head title extra-style site-style)
            [:body
             [:div.fcr-wrapper
              (news-content link title content
                            (list
                             (if (not (empty? author))
                               [:p (str "Author: " (s/join ", " author))])
                             (if (not (empty? category))
                               [:p (str "Category: " (s/join ", " category))])
                             (if comments
                               [:p [:a {:href comments :target "_blank"} "Comments"]])))
              (if source [:button.fcr-btn {:onclick done-action} "Done"])]]]))
       (catch Exception ex
         (log/error "Failed to make content for" link)))
     link)))

(defn mark-read [user-id to-positions]
  (let [pos-map (into {} to-positions)]
    (feed/update-user-attrs! user-id update :positions merge pos-map)))

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
     (let [user (feed/get-user-attrs user-id)
           feed-urls (map first (feed/make-expressions (:feeds user)))
           feeds (map feed/get-feed-attrs feed-urls)]
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

(defn build-settings [user-id extra-style]
  (let [user (feed/get-user-attrs user-id)]
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
         (s/join "\n" (:feeds user))]
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
          (->> (:styles user)
               (map #(s/join " " %))
               (s/join "\n"))]]
        [:div.fcr-bottom-buttons
         (submit-button "Save")]]]]]))

(defn save-settings [user-id feed-lines style-lines]
  (let [feeds (s/split-lines feed-lines)
        styles (map #(s/split % #"\s+")
                    (s/split-lines style-lines))
        new-feeds (->> (feed/make-expressions feeds)
                       (map first)
                       (remove @feed/feed-index))]
    (doseq [url new-feeds]
      (feed/add-feed! url))
    (feed/update-user-attrs! user-id assoc :feeds feeds :styles styles)))

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
