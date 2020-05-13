(ns snake-clj.core
  (:require [lanterna.screen :as s])
  (:require [clojure.string :refer [join]])
  (:import (clojure.lang PersistentQueue)))

(def ^:const width 20)
(def ^:const height 15)
(def screen (s/get-screen :swing))
(def initial-state {; TODO consider using a persistent dequeue for better performance retrieving head at last position
                    :snake      (conj PersistentQueue/EMPTY
                                      [2 1] [2 2] [2 3])
                    :food       [4 5]
                    :dir        :down
                    :game-over? false
                    :stats      {:turn           0
                                 :food-collected 0}})


; =========== UPDATE ==========

(defn head [s] (last s))

(def valid-directions #{:up :down :left :right})

(defn next-cell [[x y] dir]
  (case dir
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(- x 2) y]
    :right [(+ x 2) y]))

; TODO: buffer directional inputs
; This feature would involve running a separate thread or go block to listen for directional inputs
; The other thread of control would be writing directional inputs to some agent or atom, that this fn reads
(defn next-dir [dir]
  (let [in-key (s/get-key-blocking screen {:timeout 10})]
    (or
      (and
        ; ensure in-key is a direction
        (valid-directions in-key)
        ; restrict movement back into snake body
        (case [in-key dir]
          [:up :down] :down
          [:down :up] :up
          [:left :right] :right
          [:right :left] :left
          in-key))
      ; otherwise fallback to last dir
      dir)))

(defn rand-cell []
  [(* (rand-int (quot width 2)) 2)
   (rand-int height)])

(defn open-rand-cell [covered-coords]
  (let [covered-set (set covered-coords)]
    (some #(if (not (covered-set %)) %)
          (repeatedly rand-cell))))

(defn out-of-bounds? [state]
  (let [[x y] (head (:snake state))]
    (or (< x 0) (< y 0) (>= x (* width 2)) (>= y height))))

(defn overlap? [state]
  (let [s (:snake state)
        h (head s)
        t (butlast s)]
    (some #(= h %) t)))

(defn game-over? [state]
  (cond
    (out-of-bounds? state) "Out of Bounds"
    (overlap? state) "Overlap"))

(defn update-dir [state]
  (update state :dir next-dir))

(defn inc-turn-counter [state]
  (update-in state [:stats :turn] inc))

(defn move-snake [state]
  (let [snake          (:snake state)
        new-head       (next-cell (head snake) (:dir state))]
    (if (= (:food state) new-head)
      (let [new-snake (conj snake new-head)]
        (-> state
            (assoc :snake new-snake)
            (assoc :food (open-rand-cell new-snake))
            (update-in [:stats :food-collected] inc)))
      (assoc state :snake (conj (pop snake) new-head)))))

(defn update-game [state]
  (if-let [reason (game-over? state)]
    (assoc state :game-over? reason)
    (-> state
        update-dir
        inc-turn-counter
        move-snake)))


; =========== VIEW ============

(def ^:const block "██")

(defmacro clear-and-redraw [& body]
  `(do
     (s/clear screen)
     (do ~@body)
     (s/redraw screen)))

(defn draw-game [state]
  (clear-and-redraw
    (let [[fx fy] (:food state)
          snake (:snake state)]
      ; Draw Walls
      (s/put-string screen 0 height (join (repeat width block)) {:fg :yellow})
      (doseq [y (range (inc height))]
        (s/put-string screen (* width 2) y block {:fg :yellow}))
      ; Draw game elements
      (s/put-string screen fx fy block {:fg :red})
      (doseq [[sx sy] snake]
        (s/put-string screen sx sy block {:fg :green})))))

(defn draw-end-game [state]
  (clear-and-redraw
    (let [reason     (:game-over? state)
          end-turn   (get-in state [:stats :turn])
          food-count (get-in state [:stats :food-collected])
          lines ["Game Over!"
                 (str "Reason: " reason)
                 (str "End turn: " end-turn)
                 (str "Total food eaten: " food-count)
                 (str "Snake size: " (count (:snake state)))
                 "Thanks for playing! Press 'q' to quit"]]
      (doseq [[i s] (map-indexed #(vector %1 %2) lines)]
        (s/put-string screen 0 i s)))))


; ========= MAIN ==========

(defn -main
  "Runs a game of Snake in the terminal"
  [& args]
  (s/start screen)
  ; moves cursor out of the way
  (s/move-cursor screen (inc (* width 2)) height)

  ; THE GAME LOOP
  (loop [state initial-state]
    (if (:game-over? state)
      (do
        (draw-end-game state)
        (some #(= \q %)
              (repeatedly #(s/get-key-blocking screen)))
        (s/stop screen))
      (let [new-state (update-game state)]
        (draw-game new-state)
        (Thread/sleep 50)
        (recur new-state)))))
