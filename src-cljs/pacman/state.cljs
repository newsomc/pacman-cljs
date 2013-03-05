(ns pacman.state
  (:require [pacman.constants :as const]))

(def game-state (atom {:state const/WAITING
                       :audio []
                       :ghosts []
                       :ghost-specs ["#00FFDE" "#FF0000" "#FFB8DE" "#FFB847"]
                       :eaten-count 0
                       :level 0
                       :tick 0
                       :ghost-pos []
                       :user-pos []
                       :state-changed true
                       :timer-start nil
                       :last-time 0
                       :ctx nil
                       :timer nil
                       :map nil
                       :user nil
                       :stored nil
                       :block-size nil}))


