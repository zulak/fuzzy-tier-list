(ns fuzzy-tier-list.db
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def ^:private enum-type {:minion "MINION"
                :spell "SPELL"
                :weapon "WEAPON"})

(def ^:private enum-rarity {:common "COMMON"
                  :rare "RARE"
                  :epic "EPIC"
                  :legendary "LEGENDARY"
                  :free "FREE"})

(defn- load-cards []
  (json/parse-stream
   (io/reader
    (io/resource "cards.json"))))

(defn- load-tier-list []
  (let [l (-> (json/parse-stream
               (io/reader
                (io/resource "tier_list.json")))
              (get "Cards"))
        fn-get-score (fn [x] (-> (get x "Scores")
                                (first)
                                (get "Score")
                                (int)))]
    (apply hash-map (mapcat (fn [x] [(get x "Name") (fn-get-score x)]) l))))

(defn- remove-punctuation [^String s]
  (string/replace s #"[^\w ]" ""))

(defn- normalize-card-name [card]
  (string/lower-case (get card "name")))

(defn- insertion-distance [^String needle ^String haystack]
  (if (> (count needle) (count haystack))
    Integer/MIN_VALUE
    (loop [c 0
           n needle
           h haystack]
      (cond
        (empty? n) (+ c (count h))
        (empty? h) (Integer/MIN_VALUE)
        :else (if (= (first n) (first h))
                (recur (+ c 0) (rest n) (rest h))
                (recur (+ c 1) n (rest h)))))))

(defn- is-playable-card? [card]
  (let [type (get card "type")
        set (get card "set")]
    (and
     (contains? #{"MINION" "SPELL" "WEAPON"} type)
     (contains? #{"TGT" "NAXX" "GVG" "CORE" "OG" "LOE" "BRM" "EXPERT1"} set)
     (get card "collectible"))))

(defn make-card-db []
  (let [playable-cards (filter is-playable-card? (load-cards))
        scored-cards (map #(assoc % "score" (get (load-tier-list) (get % "name")))
                          playable-cards)]
    (apply hash-map (mapcat #(list (normalize-card-name %) %) scored-cards))))

(defn load-card-db []
  (json/parse-stream
   (io/reader
    (io/resource "data.json"))))

(defn fuzzy-search [cards needle]
  (if (contains? cards needle)
    [(get cards needle)]
    (->> (keys cards)
         (map #(vector (insertion-distance (string/lower-case needle) %) %))
         (filter #(>= (get % 0) 0))
         (sort-by #(get % 0))
         (map #(get % 1))
         (map #(get cards %)))))
