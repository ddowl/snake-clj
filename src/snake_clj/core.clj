(ns snake-clj.core
  (:require [lanterna.screen :as s])
  (:require [clojure.string :refer [join]]))

; =========== UPDATE ==========

(defn head [s] (last s))

(defn next-cell [[x y] dir]
  (case dir
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(- x 2) y]
    :right [(+ x 2) y]))

; TODO: buffer directional inputs
(defn next-dir [screen dir]
  (let [dir-in (or (s/get-key-blocking screen {:timeout 10}) dir)]
    (case [dir-in dir]
      [:up :down] :down
      [:down :up] :up
      [:left :right] :right
      [:right :left] :left
      dir-in)))

(defn rand-cell [w h]
  [(rand-nth (range 0 w 2))
   (rand-nth (range 0 h 2))])

(defn out-of-bounds? [state]
  (let [width (:width state)
        height (:height state)
        [x y] (head (:snake state))]
    (or (< x 0) (< y 0) (>= x (* width 2)) (>= y height))))

(defn overlap? [state]
  (let [s (:snake state)
        h (head s)
        t (butlast s)]
    (some #(= h %) t)))

(defn game-over? [state]
  (or (out-of-bounds? state) (overlap? state)))

; TODO: come up with cleaner way to compose state updates
; game over check, update dir, move snake and food
(defn update-game [state]
  (if (game-over? state)
    (assoc state :game-over? true)
    (let [snake (:snake state)
          screen (:screen state)
          width (:width state)
          height (:height state)
          dir (next-dir screen (:dir state))
          food (:food state)
          new-head (next-cell (head snake) dir)]
      (if (= food new-head)
        (assoc state
          :dir dir
          :snake (conj snake new-head)
          :food (rand-cell width height))
        (assoc state
          :dir dir
          :snake (conj (pop snake) new-head))))))

; =========== VIEW ============

(def block "██")

(defn draw-game [screen state]
  (let [[fx fy] (:food state)
        snake (:snake state)
        w (:width state)
        h (:height state)]
    ; Draw Walls
    (s/put-string screen 0 h (join (repeat w block)) {:fg :yellow})
    (doseq [y (range (inc h))]
      (s/put-string screen (* w 2) y block {:fg :yellow}))
    ; Draw game elements
    (s/put-string screen fx fy block {:fg :red})
    (doseq [[sx sy] snake]
      (s/put-string screen sx sy block {:fg :green}))))

; ========= MAIN ==========
(def screen (s/get-screen :swing))

(def width 20)
(def height 15)
(def initial-state {:food [4 5]
                    ; TODO consider using java.util.ArrayDeque. for better performance retrieving head at last position
                    :snake (conj clojure.lang.PersistentQueue/EMPTY
                                 [2 1] [2 2] [2 3])
                    :dir :down
                    :game-over? false
                    :screen screen
                    :width width
                    :height height})

(defn -main
  "Runs a game of Snake in the terminal"
  [& args]
  (s/start screen)
  (s/move-cursor screen (inc (* width 2)) height) ; move cursor out of the way
  (loop [state initial-state]
    (if (:game-over? state)
      (do
        (println "Game Over!") ; TODO: print some stats!
        (Thread/sleep 3000) ; TODO: wait for signal or something
        (s/stop screen))
      (let [new-state (update-game state)]
        (s/clear screen)
        (draw-game screen new-state)
        (s/redraw screen)
        (Thread/sleep 50)
        (recur new-state)))))
