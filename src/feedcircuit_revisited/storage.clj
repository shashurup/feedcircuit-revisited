(ns feedcircuit-revisited.storage
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]))

(defn parse-int [s] (if s (Integer. s)))

(defn write-file [filename data]
  (let [tempfilename (str filename ".temp")]
    (with-open [w (io/writer tempfilename)]
      (binding [*out* w] (pr data)))
    (fs/rename tempfilename filename)))

(defn read-file [filename]
  (if (fs/exists? filename)
    (with-open [r (java.io.PushbackReader. (io/reader filename))]
      (edn/read r))))

; a good place to add cache
(def get-data read-file)

(defn set-data [filename data]
  (write-file filename data)
  data)

(defn get-block [dir block-num]
  (get-data (str dir "/" block-num)))

(defn set-block [dir block-num data]
  (set-data (str dir "/" block-num) data))

(defn get-last-block-num [dir]
  (->> (fs/list-dir dir)
       (map fs/base-name)
       (filter #(re-matches #"[0-9]+" %))
       (map parse-int)
       (cons 0)  ; in case there are no blocks yet
       (apply max)))

(def ^:dynamic block-size "Number of items in each file" 100)

(defn get-items
  "Returns lazy sequence of items in the directory dir
   beginning from the start"
  [dir start]
  (let [start-block (quot start block-size)
        start-offset (rem start block-size)
        items (apply concat (take-while not-empty
                                        (map #(get-block dir %)
                                             (iterate inc start-block))))]
    (drop start-offset items)))


(defn get-items-backwards
  "Returns lazy sequence of items in the directory dir
   beginning from the start and moving backwards"
  [dir start]
  (let [start-block (quot start block-size)
        first-block (get-block dir start-block)
        start-offset (rem start block-size)]
    (apply concat
           (drop (- (count first-block) (inc start-offset))
                 (reverse first-block))
           (take-while not-empty
                       (map #(reverse (get-block dir %))
                            (iterate dec (dec start-block)))))))


(defn append-items!
  "Appends items to the end of the list in the directory dir"
  [dir items]
  (let [last-block-num (get-last-block-num dir)
        last-block (get-block dir last-block-num)
        new-blocks (->> (concat last-block items)
                        (partition-all block-size)
                        (map vec))
        start (+ (* last-block-num block-size)
                 (count last-block))]
    (doseq [[num block] (map-indexed vector new-blocks)]
      (set-block dir (+ last-block-num num) block))
    (range start (+ start (count items)))))

(defn get-attrs [dir]
  (get-data (str dir "/attrs")))

(defn set-attrs [dir attrs]
  (set-data (str dir "/attrs") attrs))
