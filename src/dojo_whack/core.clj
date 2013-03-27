(ns dojo-whack.core
  (:require [lanterna.screen :as s]
            [clojure.set :refer [difference]]))

(defn t [] (System/currentTimeMillis))

(defn spy
  [x label]
  (do
    (println (format "%s@%d: %s" label (t) (prn-str x)))
    x))

(def board-map [[\1 \2 \3]
                [\q \w \e]])

(def board-coords
  (set
    (for [x (range (count board-map))
          y (range (count (get board-map x)))]
      [x y])))

(def board-chars (set (flatten board-map)))

(defn new-coord
  "Gets new, unused, random keys"
  [taken-coords]
  (let [coords-left (seq (difference board-coords (set taken-coords)))]
    (when coords-left (rand-nth coords-left))))

(defn new-mole
  ([taken-coords]
   (let [enter (+ (t) (rand-int 500) 250)
         exit (+ enter 2000)]
     {:coord (new-coord taken-coords)
      :enter enter
      :exit exit}))
  ([]
   (new-mole [])))

(defn mole-coord [mole] (:coord mole))
(defn mole-char [mole] (get-in board-map (mole-coord mole)))
(defn mole-key [mole] (mole-char mole))

(defrecord Game [board next-mole score input messages])

(defn new-game
  []
  (map->Game {:board {}
              :next-mole (new-mole)
              :score 0
              :input []
              :messages []}))

(defn valid-board-input?
  [k]
  (contains? board-chars k))

(defn get-input
  [game screen]
  (when-let [k (s/get-key-blocking screen)]
    (cond
      (= :escape k) (swap! game assoc :game-over? true)
      (valid-board-input? k) (swap! game update-in [:input] conj k))))

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
      (if (whacked? (:board game) k)
        (whack game k)
        game)
      ks)
    game))

(defn mole-expired?
  [mole]
  (> (t) (:exit mole)))

(defn remove-expired-moles
  [board screen]
  (reduce
    (fn [board mole]
      (if (mole-expired? mole)
        (remove-mole board (mole-key mole))
        board))
    board
    (vals board)))

(defn activate-next-mole
  [{:keys [next-mole] :as game}]
  (if next-mole
    (-> game
      (assoc-in [:board (mole-key next-mole)] next-mole)
      (dissoc :next-mole))
    game))

(defn new-next-mole
  [{:keys [board] :as game}]
  (assoc game :next-mole (new-mole (keys board))))

(defn show-next-mole
  [{:keys [next-mole] :as game}]
  (if (and next-mole
           (>= (t) (:enter next-mole)))
    (-> game
      activate-next-mole
      new-next-mole)
    game))

(defn tick
  [game]
  (-> game
    (show-next-mole)))

(defn draw-coord
  [[x y]]
  [(+ (* y 3) 2 x) (* x 2)])

;; TODO draw-moles clear-moles
(defn clear-moles
  [screen]
  (doseq [[x y] (map draw-coord board-coords)]
    (s/put-string screen x y " ")))

(defn draw-moles
  [game screen]
  (doseq [mole (vals (:board game))]
    (let [[x y] (draw-coord (mole-coord mole))]
      (s/put-string screen x y (str (mole-char mole))))))

(defn draw-game
  [game screen]
  (clear-moles screen)
  (draw-moles game screen)
  (s/put-string screen 0 11 (str "Score: " (:score game)))
  (s/redraw screen)
  (update-in game [:board] remove-expired-moles screen))

(defn game-over?
  [game]
  (get @game :game-over? false))

(defn run-input
  [game screen]
  (while (not (game-over? game))
    (get-input game screen)))

(defn run-game
  [game screen]
  (let [input-thread (Thread. #(run-input game screen))]
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
