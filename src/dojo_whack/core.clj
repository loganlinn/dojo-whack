(ns dojo-whack.core
  (:require [lanterna.screen :as s]
            [clojure.set :refer [difference]]))

(defn t [] (System/currentTimeMillis))

(def board-map [[\1 \2 \3 \4 \5 \6 \7 \8 \9 \0]
                [\q \w \e \r \t \y \u \i \o \p]
                [\a \s \d \f \g \h \j \k \l]
                [\z \x \c \v \b \n \m]])

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
  ([t taken-coords]
   (let [enter (+ t (rand-int 500) 250)
         exit (+ enter 2000)]
     {:coord (new-coord taken-coords)
      :enter enter
      :exit exit}))
  ([t]
   (new-mole t [])))

(defn mole-coord [mole] (:coord mole))
(defn mole-char [mole] (get-in board-map (mole-coord mole)))
(defn mole-key [mole] (mole-char mole))
(defn mole-duration [{:keys [enter exit]}] (- exit enter))
(defn mole-reward
  [{:keys [enter] :as mole} t]
  (let [full-value 100
        age (- t enter)
        dur (mole-duration mole)
        p (max 0.0 (double (- 1 (/ age dur))))]
    (int (* full-value p))))

(defrecord Game [board next-mole score t input messages])

(defn new-game
  []
  (map->Game {:board {}
              :score 0
              :t (t)
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
  [{:keys [t] :as game} mole-key]
  (if-let [mole (get-in game [:board mole-key])]
    (let [reward (mole-reward mole t)]
      (println "WHACKED" mole-key "for" reward "points!")
      (-> game
        (update-in [:score] + reward)
        (update-in [:board] remove-mole mole-key)))
    game))

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
  [mole t]
  (> t (:exit mole)))

(defn expired-mole-keys
  [board t]
  (keep
    (fn [[k mole]]
      (when (mole-expired? mole t) k))
    board))

(defn remove-expired-moles
  [{:keys [board t] :as game} screen]
  (update-in game [:board]
             #(apply dissoc %
                     (expired-mole-keys % t))))

(defn activate-next-mole
  [{:keys [next-mole] :as game}]
  (if next-mole
    (-> game
      (assoc-in [:board (mole-key next-mole)] next-mole)
      (dissoc :next-mole))
    game))

(defn new-next-mole
  [{:keys [board t] :as game}]
  (assoc game :next-mole (new-mole t (keys board))))

(defn tick-moles
  [{:keys [next-mole t] :as game}]
  (cond
    (nil? next-mole) (new-next-mole game)
    (>= t (:enter next-mole)) (-> game
                                activate-next-mole
                                new-next-mole)
    :else game))

(defn draw-coord
  [[x y]]
  [(+ (* y 3) 2 x) (* x 2)])

(defn draw-moles
  [game screen]
  (doseq [mole (vals (:board game))]
    (let [[x y] (draw-coord (mole-coord mole))]
      (s/put-string screen x y (str (mole-char mole))))))

(defn draw-game
  [{:keys [score t] :as game} screen]
  (s/clear screen)
  (draw-moles game screen)
  (s/put-string screen 0 11 (str "Score: " score))
  (s/redraw screen)
  (remove-expired-moles game screen))

(defn game-over?
  [game]
  (get @game :game-over? false))

(defn run-input
  [game screen]
  (while (not (game-over? game))
    (get-input game screen)))

(defn tick-time
  [game]
  (assoc game :t (t)))

(defn run-game
  [game screen]
  (while (not (game-over? game))
    (let [{:keys [input]} @game]
      (swap! game
             (if (seq input)
               #(-> %
                  (assoc :input [])
                  (process-input input))
               #(-> %
                  (tick-time)
                  (tick-moles)
                  (draw-game screen))))
      (Thread/sleep 50))))

(defn main
  [screen-type]
  (let [screen (s/get-screen screen-type)
        game (atom (new-game))
        input-thread (Thread. #(run-input game screen))]
    (s/in-screen screen
                 (s/put-string screen 0 0 "Welcome to Dojo Whack-a-Mole!")
                 (s/put-string screen 0 1 "Press any key to start...")
                 (s/redraw screen)
                 (s/clear screen)
                 (s/get-key-blocking screen)
                 (.start input-thread)
                 (run-game game screen))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type)))
