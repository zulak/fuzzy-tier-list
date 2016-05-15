(ns fuzzy-tier-list.core
  (:require [fuzzy-matching.db :as db]
            [lanterna.terminal :as t])
  (:gen-class))

(defn- format-card [{:strs [cost score name rarity]}]
  (format "[%d] %s (%s) - %d" cost name (first rarity) score))

(defn- handle-input [ch]
  (cond
    (= ch :enter) [:clear]
    (= ch :backspace) [:backspace]
    (keyword? ch) nil
    (= ch \) [:exit]
    :else [:print ch]))

(defn- draw [state]
  (let [term (:term @state)
        [x y] (:pos @state)
        query (apply str (:query @state))]
    (t/clear term)
    (t/put-string term query)
    (when (>= (count query) 3)
      (let [results (take 5 (db/fuzzy-search (:cards @state) query))]
        (loop [res results
               line 2]
          (when (seq res)
            (do
              (t/move-cursor term 0 line)
              (t/put-string term (format-card (first res)))
              (recur (rest res) (inc line)))))
        (t/move-cursor term x y)))))

(defn- on-enter [state]
  (let [term (:term @state)
        cards (:cards @state)]
    (reset! state {:cards cards
                   :term term
                   :pos [0 0]
                   :query []
                   :continue true})))

(defn- on-char [state ch]
  (let [term (:term @state)]
    (swap! state (fn [s]
                   (let [[x y] (:pos s)]
                     (->
                      (assoc s :query (conj (:query s) ch))
                      (assoc :pos [(+ 1 x) y])))))))

(defn- on-backspace [state]
  (let [term (:term @state)
        [x y] (:pos @state)
        query (:query @state)]
    (when (> x 0)
      (swap! state (fn [s]
                     (-> (assoc s :pos [(- x 1) y])
                         (update :query pop))
                     )))))

(defn run [term card-db]
  (let [state (atom {:cards card-db
                     :term term
                     :pos [0 0]
                     :query []
                     :continue true})]
    (while (:continue @state)
      (do
        (let [in (handle-input (t/get-key-blocking term))]
          (condp = (first in)
            :clear (on-enter state)
            :backspace (on-backspace state)
            :print (on-char state (second in))
            :exit (swap! state #(assoc % :continue nil))
            ))
        (draw state)
        ))))

(defn -main []
  (let [term (t/get-terminal :text)
        cards (db/load-card-db)]
    (t/in-terminal term
                   (run term cards)))
  (println "And done."))
