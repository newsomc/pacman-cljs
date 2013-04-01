(ns pacman.state
  (:require [pacman.constants :as const]
            [pacman.user :as user]))

;; "In a functional language, the worst thing you can do is create a large "struct" 
;;  containing all the data you think you might need for an entity."
;;  http://prog21.dadgum.com/25.html
;;
;; Well, let's just do that anyway!


;; ------------------------------------------------------------------------------------------------
;; Direction key/map
;; ------------------------------------------------------------------------------------------------

;; Massive bug here! Up and down are switched for some reason. Look into this. 


