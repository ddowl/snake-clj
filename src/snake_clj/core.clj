(ns snake-clj.core
  (:require [clojure.term.colors :as tc]))

; =========== UPDATE ==========

(defn update-game [state] state)

; =========== VIEW ============

(def block "██")

(defn print-game [state]
  (let [food (:food state)
        snake (get-in state [:snake :body])
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
                :snake {:body [[2 1] [2 2] [2 3]]
                        :dir :down}
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