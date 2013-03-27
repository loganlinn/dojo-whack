(ns dojo-whack.core
  (:require [lanterna.screen :as s]))

(defn t [] (System/currentTimeMillis))

(defn spy
  [x label]
  (do
    (println (format "%s@%d: %s" label (t) (prn-str x)))
    x))

(defn new-mole
  []
  (let [enter (+ (t) (rand-int 1000))
        exit (+ enter 3000)]
    {:char (rand-int 10)
     :enter enter
     :exit exit}))

(defn mole-key [mole] (:char mole))

(defrecord Game [board next-mole score input messages])

(defn new-game
  []
  (map->Game {:board {}
              :next-mole (new-mole)
              :score 0
              :input []
              :messages []}))

(defn get-input
  [game screen]
  (when-let [k (s/get-key-blocking screen)]
    (swap! game update-in [:input] conj k)))

(defn remove-mole
  [board mole-key]
  (dissoc board mole-key))

(defn whack
  [game mole-key]
  (println "WHACKED " mole-key)
  (-> game
    (update-in [:board] remove-mole mole-key)
    (update-in [:score] + 100)))

(defn whacked?
  [board k]
  (contains? board k))

(defn process-input
  [game [k & ks]]
  (if k
    (recur
      (let [k (Integer. (str k))]
        (if (whacked? (:board game) k)
          (whack game k)
          game))
      ks)
    game))

(defn mole-expired?
  [mole]
  (> (t) (:exit mole)))

(defn remove-old-moles
  [board screen]
  (reduce
    (fn [board mole]
      (if (mole-expired? mole)
        (do
          ;(s/put-string screen 0 (mole-key mole) " ")
          (remove-mole board (mole-key mole)))
        board))
    board
    (vals board)))

(defn add-mole
  [board mole]
  (assoc board (mole-key mole) mole))

(defn show-next-mole
  [{:keys [next-mole] :as game}]
  (if (>= (t) (:enter next-mole))
    (-> game
      (update-in [:board] add-mole next-mole)
      (assoc :next-mole (new-mole)))
    game))

(defn tick
  [game]
  (-> game
    (show-next-mole)))

(defn char-pos [char] char)

;; TODO draw-moles clear-moles
(defn draw-game
  [game screen]
  (doseq [n (range 10)]
    (s/put-string screen 0 n " "))
  (doseq [[char mole] (:board game)]
    (s/put-string screen 0 (char-pos char) (str char)))
  (s/put-string screen 0 11 (str "Score: " (:score game)))
  (s/redraw screen)
  (update-in game [:board] remove-old-moles screen))

(defn game-over?
  [game]
  false)

(defn run-game
  [game screen]
  (let [input-thread (Thread. #(while true (get-input game screen)))]
    (.start input-thread)
    (while (not (game-over? game))
      (let [{:keys [input]} @game]
        (swap! game
               (if (seq input)
                 #(-> %
                    (assoc :input [])
                    (process-input input))
                 #(-> %
                    (tick)
                    (draw-game screen))))
        (Thread/sleep 50)))))

(defn main
  [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (s/put-string screen 0 0 "Welcome to Dojo Whack-a-Mole!")
                 (s/put-string screen 0 1 "Press any key to start...")
                 (s/redraw screen)
                 (s/get-key-blocking screen)
                 (s/put-string screen 0 0 "                              ")
                 (s/put-string screen 0 1 "                              ")
                 (run-game (atom (new-game)) screen))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type)))
