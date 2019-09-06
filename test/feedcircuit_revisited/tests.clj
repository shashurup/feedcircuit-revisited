(ns feedcircuit-revisited.tests
  (:require  [clojure.test :as t]
             [feedcircuit-revisited.content :as content]
             [feedcircuit-revisited.jsoup :as jsoup]
             [clojure.zip :as zip]))

(t/deftest find-nodes-test
  (defn find-nodes' [html pred]
    (->> html jsoup/parse-string (content/find-nodes pred) (map zip/node)))

  (t/is (= (find-nodes' "abc" string?)
           '("abc")))
  (t/is (= (find-nodes' "abc<p>def</p>ghi" string?)
           '("abc" "def" "ghi")))
  (t/is (= (find-nodes' "abc<p>def</p>ghi" #(and (content/element? %) (= (content/tag %) :p)))
           '([:p nil "def"])))
)

(t/deftest find-tags-test
  (defn find-tags' [html pred]
    (->> html jsoup/parse-string (content/find-tags pred) (map zip/node)))

  (t/is (= (find-tags' "abc<p>def</p>ghi" #{:p})
           '([:p nil "def"])))
  (t/is (= (find-tags' "<h1>abc</h1><p>def</p><span>ghi</span>" #{:p :h1})
           '([:h1 nil "abc"] [:p nil "def"])))
)

(t/deftest find-body-test
  (t/is (= (->> "abc"
                jsoup/parse-string
                content/find-body)
           [:body nil "abc"]))
  (t/is (nil? (content/find-body "abc")))
  )

(t/deftest remove-h1-test
  (t/is (= (->> "<h1>abc</h1>def"
                jsoup/parse-string
                content/remove-h1)
           [:html nil [:head nil] [:body nil "def"]]))
  (t/is (= (->> "abc"
                jsoup/parse-string
                content/remove-h1)
           [:html nil [:head nil] [:body nil "abc"]]))
)
