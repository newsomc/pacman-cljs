(ns pacman.user
  (:require [pacman.constants :as const]))

(def user-state (atom {:position nil
                       :direction nil
                       :eaten nil
                       :due nil
                       :lives nil
                       :score 5
                       :key-map {}}))

(def key-map {(:ARROW_LEFT const/KEY) (:left const/game-const)
              (:ARROW_UP const/KEY) (:up const/game-const)
              (:ARROW_RIGHT const/KEY) (:right const/game-const)
              (:ARROW_DOWN const/KEY) (:down const/game-const) })


(defn add-score [n-score])

(defn the-score [])

(defn lose-life [])

(defn get-lives [])

(defn init-user [])

(defn new-level [])

(defn reset-position [])

(defn reset [])

(defn key-down [e])

(defn get-new-coord [dir current])

(defn on-whole-square [x])

(defn print-to-coord [x])

(defn next-square [x dir])

(defn next [pos dir])

(defn on-grid-square [pos])

(defn is-on-same-plane [due dir])

(defn move [ctx])

(defn is-mid-square [x])

(defn calc-angle [dir pos])

(defn draw-dead [ctx amount])

(defn draw [ctx])


