(ns snake-clj.core.desktop-launcher
  (:require [snake-clj.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. snake-clj-game "snake-clj" 800 600)
  (Keyboard/enableRepeatEvents true))
