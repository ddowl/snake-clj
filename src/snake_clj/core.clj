(ns snake-clj.core
  (:require [clojure.term.colors :as tc]))

; =========== UPDATE ==========

(defn update-game [state] state)

; =========== VIEW ============

(def block "██")

(defn print-row [n]
  (dorun (repeatedly n #(print (tc/yellow block))))
  (println))

(defn print-grid [w h]
  (dorun (repeatedly h #(print-row w))))

(defn clear [] (print (str (char 27) "[2J")))

; ========= MAIN ==========

(defn -main
  "Runs a game of Snake in the terminal"
  [& args]
  (let [grid-x 15
        grid-y 10]
    (loop [state {:food [0 0]
                  :snake {:body [[2 1] [2 2] [2 3]]
                          :dir :up}
                  :game-over? false}]
      (if (:game-over? state)
        (println "Game Over!")
        (let [new-state (update-game state)]
          (clear)
          (print-grid grid-x grid-y)
          (Thread/sleep 100)
          (recur new-state))))))