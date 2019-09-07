(ns feedcircuit-revisited.tests
  (:require  [clojure.test :as t]
             [feedcircuit-revisited.content :as content]
             [feedcircuit-revisited.jsoup :as jsoup]
             [clojure.zip :as zip]
             [hiccup.core :as html]))

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

(t/deftest html-zipper-test
  (defn list-nodes [html pred]
    (->> html
         (content/html-zipper pred)
         content/node-seq
         (map zip/node)
         vec))
  (t/is (= (list-nodes [:body nil "abc" [:p nil "def"]] nil)
           [[:body nil "abc" [:p nil "def"]]
            "abc"
            [:p nil "def"]
            "def"]))
  (t/is (= (list-nodes [:body nil "abc" [:script nil "def"]]
                       (comp nil? #{:script} content/tag))
           [[:body nil "abc" [:script nil "def"]]
            "abc"
            [:script nil "def"]]))
)

(t/deftest content-element-test
  (t/is (not (content/content-element? "abc")))
  (t/is (not (content/content-element? [:script nil])))
  (t/is (content/content-element? [:div nil]))
  (t/is (content/content-element? [:p nil]))
)

(t/deftest text-size-recursively-test
  (defn calc-text-size [html]
    (->> html
         jsoup/parse-string
         content/text-size-recursively))
  (t/is (= (calc-text-size "abc") 3))
  (t/is (= (calc-text-size "abc<p>def") 6))
  (t/is (= (calc-text-size "abc<p>def</p><script>bla bla bla</script>") 6))
)
