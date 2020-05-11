(ns snake-clj.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]))

(defn move
  [entity direction]
  (case direction
    :down (update entity :y dec)
    :up (update entity :y inc)
    :left (update entity :x dec)
    :right (update entity :x inc)
    nil))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (add-timer! screen :move-snake 0 0.5)
    (update! screen :renderer (stage) :camera (orthographic))
    (assoc (texture "Clojure-icon.png")
      :x 50 :y 50 :width 100 :height 100
      :angle 45 :origin-x 0 :origin-y 0))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :move-snake (move (first entities) :right)
      nil))

  :on-resize
  (fn [screen entities]
    (height! screen 600))

  :on-render
  (fn [screen entities]
    (clear!)
    (render! screen entities))

  :on-key-down
  (fn [screen entities]
    (cond
      (= (:key screen) (key-code :dpad-up))
      (move (first entities) :up)
      (= (:key screen) (key-code :dpad-down))
      (move (first entities) :down)
      (= (:key screen) (key-code :dpad-right))
      (move (first entities) :right)
      (= (:key screen) (key-code :dpad-left))
      (move (first entities) :left)))

  :on-touch-down
  (fn [screen entities]
    (cond
      (> (game :y) (* (game :height) (/ 2 3)))
      (move (first entities) :up)
      (< (game :y) (/ (game :height) 3))
      (move (first entities) :down)
      (> (game :x) (* (game :width) (/ 2 3)))
      (move (first entities) :right)
      (< (game :x) (/ (game :width) 3))
      (move (first entities) :left))))



(defgame snake-clj-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
