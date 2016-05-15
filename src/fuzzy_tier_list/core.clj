(ns fuzzy-tier-list.core
  (:require [fuzzy-tier-list.db :as db]
            [lanterna.terminal :as t])
  (:gen-class))

(defn- format-card [{:strs [cost score name rarity]}]
  (format "[%d] %s (%s) - %d" cost name (first rarity) score))

(defn- draw [state term]
  (let [query (apply str (:query state))
        results (when (>= (count query) 3)
                  (take 5 (db/fuzzy-search (:cards state) query)))]
    (t/clear term)
    (loop [res results
           line 2]
      (when (seq res)
        (do
          (t/move-cursor term 0 line)
          (t/put-string term (format-card (first res)))
          (recur (rest res) (inc line)))))
    (t/move-cursor term 0 0)
    (t/put-string term query)))

(defn- on-enter [state]
  (assoc state :query []))

(defn- on-char [state input]
  (update state :query #(conj % input)))

(defn- on-backspace [state]
  (if-not (empty? (:query state))
    (update state :query pop)
    state))

(defn- on-exit [state]
  (assoc state :continue false))

(defn- new-state []
  {:cards (db/load-card-db)
   :query []
   :continue true
   :input nil})

(defn get-input [state term]
  (assoc state :input (t/get-key-blocking term)))

(defn handle-input [state input]
  (letfn [(is-printable? [x]
            (and (char? x) (some #{(int x)} (range 32 127))))]
    (cond
      (= input :enter) (on-enter state)
      (= input :backspace) (on-backspace state)
      (is-printable? input) (on-char state input)
      :else state)))

(defn run-loop
  [term state]
  (loop [state state]
    (when (:continue state)
      (draw state term)
      (let [input (:input state)]
        (if (nil? input)
          (recur (get-input state term))
          (recur (handle-input (dissoc state :input) input)))))))

(defn main
  [term-type block?]
  (letfn [(go []
              (let [term (t/get-terminal term-type)]
                (t/in-terminal term
                               (run-loop term (new-state)))))]
    (if block?
      (go)
      (future (go)))))

(defn -main []
  (main :text true))
