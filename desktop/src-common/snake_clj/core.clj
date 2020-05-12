(ns snake-clj.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]))

(declare snake-clj-game main-screen)

(def ^:const block-size 50)
(def inc-block (partial + block-size))
(def dec-block (partial + block-size))

(defn move
  [entity direction]
  (case direction
    :down (update entity :y dec-block)
    :up (update entity :y inc-block)
    :left (update entity :x dec-block)
    :right (update entity :x inc-block)
    nil))

(defn block [grid-x grid-y color]
  (let [x (* grid-x block-size)
        y (* grid-y block-size)]
    (shape :filled
           :set-color color
           :rect x y block-size block-size)))
(defn food [grid-x grid-y]
  (block grid-x grid-y (color :red)))

(defn snake-block [grid-x grid-y]
  (block grid-x grid-y (color :green)))

(defscreen main-screen
  :on-show
  (fn [screen _]
    (let [screen (update! screen
                          :camera (orthographic)
                          :renderer (stage))
          tiles-w (/ (game :width) block-size)
          tiles-h (/ (game :height) block-size)
          grid-w (* tiles-w block-size)
          grid-h (* tiles-h block-size)]

      ;(add-timer! screen :game-loop 0.5 0.1)
      (width! screen grid-w)
      (height! screen grid-h)
      [(food 3 4) (snake-block 1 2)]))



  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :game-loop (move (first entities) :right)
      nil))

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
      (move (first entities) :left))))



(defgame snake-clj-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))
