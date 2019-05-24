(ns feedcircuit-revisited.ui
  (:require [clojure.string :as s]
            [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.content :as content]
            [feedcircuit-revisited.auth :as auth]))

(def page-size 16)

(defn get-next-positions [user-items]
  (into {} (map #(vector (:feed %) (inc (:num %))) user-items)))

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

(defn ch-id [idx] (str "ch" idx))

(defn item-checkbox [idx class value]
  [:input {:type "checkbox"
           :id (ch-id idx)
           :class (str "item-check " class)
           :name "selected-item"
           :value value}])

(defn news-item [url title summary mark]
  [:div.fcr-news-item
   [:div.fcr-news-header
    [:a.fcr {:href url
             :target "_blank"} title]]
   summary "&nbsp;" mark])

(defn head [title]
  [:head
   [:title title]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
   [:link {:rel "stylesheet" :type "text/css" :href "style.css"}]])

(defn submit-button
  ([caption] (submit-button caption false))
  ([caption disabled]
   [:input (merge {:class (str "fcr-btn"
                               (if disabled " disabled" ""))
                   :type "submit"
                   :value caption}
                  (if disabled {:disabled true} {}))]))

(defn navbar [user-id selected]
  [:div.fcr-nav-bar
   (if selected
     (list [:a.fcr {:href "/"} "Feed"] " | "
           [:span "Selected"] " | ")
     (list [:span "Feed"] " | "
           [:a.fcr {:href "/selected"} "Selected"] " | "))
   [:div.fcr-menu
    [:a.fcr {:href "/extra-links"} "..."]
    [:div.fcr-menu-items
     [:div.fcr-menu-item "Logged in as:" [:br] user-id]
     [:div.fcr-menu-item [:a.fcr {:href "/settings"} "Settings"]]
     [:div.fcr-menu-item [:a.fcr {:href "/logout"} "Logout"]]]]])

(defn build-extra-links [user-id]
  [:html
   (head "Feedcircuit")
   [:body
    [:p "Logged in as " user-id]
    [:p [:a {:href "/settings"} "Settings"]]
    [:p [:a {:href "/logout"} "Logout"]]]])

(defn build-feed [user-id item-count]
  (let [user (feed/get-user-attrs user-id)
        items (feed/get-user-items user item-count)
        next-positions (get-next-positions items)]
    [:html
     (head "Feedcircuit")
     [:body
      (navbar user-id false)
      [:form {:action "next" :method "POST"}
       [:div#fcr-content
        (for [[idx {title :title
                    summary :summary
                    content :content
                    feed :feed
                    ord-num :num}] (map-indexed vector items)]
          (list (item-checkbox idx "select-item-check" (str ord-num "," feed))
                (news-item (str "read?id=" ord-num "," feed)
                           title
                           (or summary content)
                           [:label {:class "item-check"
                                    :for (ch-id idx)} (bookmark-icon-svg)])))
        (if (empty? items)
          [:p.fcr-no-more-items "No more items"])
        (for [[feed pos] next-positions]
          [:input {:type "hidden"
                   :name "next-position"
                   :value (str pos "," feed)}])
        (if-not (empty? items)
          (submit-button (str "Next " page-size " >>")))]]]]))

(defn build-selected [user-id]
  (let [items (feed/get-selected-items user-id)]
    [:html
     (head "Feedcircuit, selected items")
     [:body
      (navbar user-id true)
      [:form {:action "archive" :method "POST"}
       [:div#fcr-content
        (for [[idx {title :title
                    summary :summary
                    content :content
                    ord-num :num}] (map-indexed vector items)]
          (list (item-checkbox idx "archive-item-check" ord-num)
                (news-item (str "read?id=" ord-num ",")
                           title
                           (or summary content)
                           [:label {:class "item-check"
                                    :for (ch-id idx)} (checkbox-svg)])))
        (if (empty? items)
          [:p.no-more "No more items"])
        (if-not (empty? items)
          (submit-button "Archive selected"))]]]]))

(defn get-item-link [item]
  (let [link (:link item)]
    (if (coll? link) (first link) link)))

(defn build-content [user-id item-id feed url]
  (let [dir (if (empty? feed)
              (feed/user-dir user-id)
              (get @feed/feed-dir feed))
        item (if item-id
               (first (feed/get-items dir item-id))
               {:link url})
        link (get-item-link item)
        content (or (:content item) (content/detect link (:summary item)))
        title (:title item)
        author (:author item)
        category (:category item)]
    (if content
      [:html
       (head title)
       [:body
        [:div#fcr-content
         (news-item link
                    title
                    content
                    "")
         [:div.fcr-article-footer
          (if (not (empty? author))
            [:p (str "Author: " (s/join ", " author))])
          (if (not (empty? category))
            [:p (str "Category: " (s/join ", " category))])]]]]
      link)))

(defn mark-read [user-id to-positions selected]
  (let [user (feed/get-user-attrs user-id)]
    (-> (update-in user [:unread] into (feed/select-items! user-id selected))
        (update-in [:positions] merge (into {} to-positions))
        (feed/update-user-attrs!))))

(defn archive-items [user-id item-ids]
  (let [user (feed/get-user-attrs user-id)]
    (feed/update-user-attrs!
     (update user :unread #(apply disj % item-ids)))))

(defn build-settings [user-id]
  (let [user (feed/get-user-attrs user-id)]
    [:html
     (head "Feedcircuit settings")
     [:body
      [:div#fcr-content
       [:p
        "Each line in the list below defines a news source to constitute your feed. "
        "In the simplest case it is just an URL of RSS or Atom feed. "]
       [:p [:code "http://example.com/rss.xml"]]
       [:p "More sofisticated setup allows you to filter news source by author and category. "
           "For instance"]
       [:p [:code "http://example.com/atom.xml Sport, Science, John Doe"]]
       [:p "selects only entries in Sport and Science categories and by John Doe. Finally,"]
       [:p [:code "http://example.com/rss.xml !Politics"]]
       [:p "selects everything except Politics category."]
       [:form {:action "save-settings" :method "POST"}
        [:textarea#fcr-settings-text {:name "feeds"}
         (s/join "\n" (:feeds user))]
        [:br]
        (submit-button "Save")
        [:a.fcr-btn.fcr-btn-right {:href "./"} "Back to the feed"]]]]]))

(defn save-settings [user-id feed-lines]
  (let [user (feed/get-user-attrs user-id)
        feeds (s/split-lines feed-lines)
        new-feeds (->> feeds
                       (map feed/parse-feed-expression)
                       (map first)
                       (filter #(not (get @feed/feed-dir %))))]
    (doseq [url new-feeds]
      (feed/add-feed! url))
    (feed/update-user-attrs! (assoc user :feeds feeds))))

(defn build-login-options []
  [:html
   (head "Welcome to Feedcircuit")
   [:body
    [:div.hv-center
     [:div#fcr-login-options
      [:p "Sign in with:"]
      (for [{title :title
             icon :icon
             url :url} (auth/get-providers)]
        [:p [:a.fcr {:href url}
             (when icon
               [:img {:src icon}])
             title]])]]]])
