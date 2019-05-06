(ns feedcircuit-revisited.ui
  (:require [feedcircuit-revisited.feed :as feed]
            [feedcircuit-revisited.content :as content]))

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
  [:div {:class "news-item"}
   [:div {:class "news-header"}
    [:a {:href url
         :target "_blank"} title]]
   summary "&nbsp;" mark])

(defn build-feed [user-id item-count]
  (let [user (feed/get-user-attrs user-id)
        items (feed/get-user-items user item-count)
        next-positions (get-next-positions items)]
    [:html
     [:head
      [:title "Feedcircuit"]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
      [:link {:rel "stylesheet" :type "text/css" :href "style.css"}]]
     [:body
      [:form {:action "next" :method "POST"}
       [:div {:class "news-list"}
        (for [[idx {title :title
                    summary :summary
                    content :content
                    link :link
                    feed :feed
                    ord-num :num}] (map-indexed vector items)]
          (list (item-checkbox idx "select-item-check" (str ord-num "," feed))
                (news-item (str "read?id=" ord-num "," feed)
                           title
                           (or summary content)
                           [:label {:class "item-check"
                                    :for (ch-id idx)} (bookmark-icon-svg)])))
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
  (let [items (feed/get-selected-items user-id)]
    [:html
     [:head
      [:title "Feedcircuit, selected items"]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
      [:link {:rel "stylesheet" :type "text/css" :href "style.css"}]]
     [:body
      [:form {:action "archive" :method "POST"}
       [:div {:class "news-list"}
        (for [[idx {title :title
                    summary :summary
                    content :content
                    link :link
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
          [:input {:class "nav-btn"
                   :type "submit"
                   :value "Archive selected"}])
        [:a {:class "nav-btn nav-btn-right"
             :href "./"} "Back to the feed"]]]]]))

(defn build-content [user-id item-id feed url]
  (let [dir (if (empty? feed)
              (feed/user-dir user-id)
              (get @feed/feed-dir feed))
        item (if item-id
               (first (feed/get-items dir item-id))
               {:link [url]})
        link (first (:link item))
        content (or (:content item) (content/detect link (:summary item)))
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
  (let [user (feed/get-user-attrs user-id)]
    (-> (update-in user [:unread] into (feed/select-items! user-id selected))
        (update-in [:positions] merge (into {} to-positions))
        (feed/update-user-attrs!))))

(defn archive-items [user-id item-ids]
  (let [user (feed/get-user-attrs user-id)]
    (feed/update-user-attrs!
     (update user :unread #(apply disj % item-ids)))))
