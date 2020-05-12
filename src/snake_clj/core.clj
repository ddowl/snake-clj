(ns snake-clj.core
  (:require [clojure.term.colors :as tc]))

; =========== UPDATE ==========

(defn move-snake [state]
  (let [snake (:snake state)
        dir (:dir state)
        moved-snake (map (fn [[x y]]
                           (case dir
                             :up [x (dec y)]
                             :down [x (inc y)]
                             :left [(dec x) y]
                             :right [(inc x) y]))
                         snake)]
    (assoc state :snake moved-snake)))


(defn eat-food [state] state)

(defn end-game [state] state)

(defn update-game [state]
  (-> state
      move-snake
      eat-food
      end-game))

; =========== VIEW ============

(def block "██")

(defn print-game [state]
  (let [food (:food state)
        snake (:snake state)
        width (:width state)
        height (:height state)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (let [curr-coords [x y]]
          (cond
            (= curr-coords food) (print (tc/red block))
            (some #(= curr-coords %) snake) (print (tc/green block))
            :else (print (tc/yellow block)))))
      (println))))

(defn clear [] (print (str (char 27) "[2J")))

; ========= MAIN ==========

(defn -main
  "Runs a game of Snake in the terminal"
  [& args]
  (loop [state {:food [4 5]
                :snake [[2 1] [2 2] [2 3]]
                :dir :down
                :game-over? false
                :width 15
                :height 10}]
    (if (:game-over? state)
      (println "Game Over!")
      (let [new-state (update-game state)]
        (clear)
        (print-game new-state)
        (Thread/sleep 100)
        (recur new-state)))))