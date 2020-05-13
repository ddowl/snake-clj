(ns snake-clj.core
  (:require [lanterna.screen :as s])
  (:require [clojure.string :refer [join]]))

; TODO: break out main, update, view, constant files and import necessary values
(declare screen width height)

; =========== UPDATE ==========

(defn head [s] (last s))

(defn next-cell [[x y] dir]
  (case dir
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(- x 2) y]
    :right [(+ x 2) y]))

; TODO: buffer directional inputs
(defn next-dir [dir]
  (let [dir-in (or (s/get-key-blocking screen {:timeout 10}) dir)]
    (case [dir-in dir]
      [:up :down] :down
      [:down :up] :up
      [:left :right] :right
      [:right :left] :left
      dir-in)))

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

; TODO: come up with cleaner way to compose state updates
; game over check, update dir, move snake and food
(defn update-game [state]
  (if-let [reason (game-over? state)]
    (assoc state :game-over? reason)
    (let [snake (:snake state)
          dir (next-dir (:dir state))
          food (:food state)
          turn (get-in state [:stats :turn])
          food-collected (get-in state [:stats :food-collected])
          new-head (next-cell (head snake) dir)]
      (if (= food new-head)
        (let [new-snake (conj snake new-head)]
          (assoc state
            :dir dir
            :snake new-snake
            :food (open-rand-cell new-snake)
            :stats {:turn (inc turn)
                    :food-collected (inc food-collected)}))
        (assoc state
          :dir dir
          :snake (conj (pop snake) new-head)
          :stats {:turn (inc turn)
                  :food-collected food-collected})))))

; =========== VIEW ============

(def ^:const block "██")

(defn draw-game [state]
  (let [[fx fy] (:food state)
        snake (:snake state)]
    ; Draw Walls
    (s/put-string screen 0 height (join (repeat width block)) {:fg :yellow})
    (doseq [y (range (inc height))]
      (s/put-string screen (* width 2) y block {:fg :yellow}))
    ; Draw game elements
    (s/put-string screen fx fy block {:fg :red})
    (doseq [[sx sy] snake]
      (s/put-string screen sx sy block {:fg :green}))))

; ========= MAIN ==========

(def ^:const width 20)
(def ^:const height 15)
(def screen  (s/get-screen :swing))
(def initial-state {:food [4 5]
                    ; TODO consider using java.util.ArrayDeque. for better performance retrieving head at last position
                    :snake (conj clojure.lang.PersistentQueue/EMPTY
                                 [2 1] [2 2] [2 3])
                    :dir :down
                    :game-over? false
                    :stats {:turn 0
                            :food-collected 0}})

(defn print-end-game [state]
  (let [reason (:game-over? state)
        end-turn (get-in state [:stats :turn])
        food-count (get-in state [:stats :food-collected])]
    (println "Game Over!")
    (println "Reason:" reason)
    (println "End turn:" end-turn)
    (println "Total food eaten:" food-count)
    (println "Snake size:" (count (:snake state)))))


(defn -main
  "Runs a game of Snake in the terminal"
  [& args]
  (s/start screen)
  (s/move-cursor screen (inc (* width 2)) height) ; move cursor out of the way
  (loop [state initial-state]
    (if (:game-over? state)
      (do
        (print-end-game state)
        ; TODO: wait for signal or something
        (s/stop screen))
      (let [new-state (update-game state)]
        (s/clear screen)
        (draw-game new-state)
        (s/redraw screen)
        (Thread/sleep 50)
        (recur new-state)))))
