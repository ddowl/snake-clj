(ns snake-clj.core
  (:require [clojure.term.colors :as tc]))

(def block "██")

(defn print-row [n]
  (dorun (repeatedly n #(print (tc/yellow block))))
  (println))

(defn print-grid [w h]
  (dorun (repeatedly h #(print-row w))))

(defn -main
  "Runs a game of Snake in the terminal"
  [& args]
  (let [grid-x 15
        grid-y 10]
    (print-grid grid-x grid-y)))

