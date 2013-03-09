(ns pacman.core
  (:require [pacman.constants :as const]
            [pacman.ghost :as ghost]
            [pacman.user :as user]
            [pacman.audio :as audio]
            [pacman.gamemap :as gamemap]
            [pacman.helpers :as helper]
            [pacman.state :as state]))

;;------------------------------------------------------------------------------------------ 
;; App
(defn draw-score 
  [text position]
  (helper/update-game :ctx (.fillStyle (:ctx @state/game-state) "#FFFFFF")))

(defn dialog [text ctx]
  (set! (. ctx  -fillStyle) (str "#FFFFFF"))
  ;; fill font!
  (let [dialog-width (.-width (.measureText ctx text))
        map-width (alength (aget const/game-map 0))
        map-height (alength const/game-map)
        x (/ (- dialog-width (* map-width (:block-size @state/game-state))) 2)]
    (.fillText ctx text x (+ (* map-height 10) 8))))

(defn sound-disabled 
  "Local storage dependent."
  [])

(defn start-level 
  "Start new level. "
  [])

(defn start-new-game [])

(defn key-down [e])

(defn lose-life [])

(defn set-state [n-state])

(defn collided [user ghost]
  (< (+ (Math/sqrt (Math/pow (- (:x ghost) (:x user)) 2) 
                   (Math/pow (- (:y ghost) (:y user)) 2))) 10))

(defn draw-footer [])

(defn redraw-block [pos])

(defn main-draw 
  "Warning...This probably doesn't work! i.e. AT ALL!"
  []
  ;; loop 1
  (doseq [ghost (:ghosts @state/game-state)]
      (swap! state/game-state update-in [:ghost-pos] conj ghost)
      (ghost/move (:ctx @state/game-state)))
  
  (swap! state/game-state update-in [:user] (user/move (:ctx @state/game-state)))

  ;; loop 2
  (doseq [ghost (:ghosts @state/game-state)]
    (redraw-block (:old (:ghost-pos @state/game-state))))
  
  ;; this may not be the right param for redraw-block
  (redraw-block (:old (:user @state/game-state)))

  ;; loop 3
  (doseq [ghost (:ghosts @state/game-state)]
    (ghost/draw ghost (:ctx @state/game-state)))

  (user/draw (:user @state/game-state))
  
  (swap! state/game-state update-in [:user-pos] conj ["new"])

  ;; loop 4
  (doseq [ghost (:ghosts @state/game-state)]
    (if (collided (:user-pos @state/game-state) (let ([pos] ())))
      (if (ghost/is-vulnerable? ghost)
        (audio/play "eatghost")
        (ghost/eat ghost)
        (swap! state/game-state update-in [:eaten-count] (fnil inc 0))
        (swap! state/game-state assoc-in [:n-score] (* (:eaten-count @state/game-state) 50))
        (user/add-score (:n-score @state/game-state))
        (set-state (:eaten-pause const/game-const))
        (swap! state/game-state assoc-in [:timer-start] (:tick @state/game-state)))
      (if (ghost/is-dangerous? ghost)
        (audio/play "die")
        (set-state (:dying const/game-const))
        (swap! state/game-state assoc-in [:timer-start] (:tick @state/game-state))))))

(defn start-new-game []
  (swap! state/game-state assoc-in [:state-changed] false)
  (gamemap/draw (:ctx @state/game-state))
  (dialog "Press N to start a New game" (:ctx @state/game-state)))

(defn game-playing []
  (gamemap/draw (:ctx @state/game-state)) 
  (set-state (:playing const/game-const)))

(defn game-dying 
  "Uhhh. Make sure this actually frigging works."
  []
  (if (> (- (:tick (@state/game-state)) (:timer-start @state/game-state)) (/ (const/FPS) 3)) 
    (lose-life)
    (do
      (redraw-block (:user-pos @state/game-state))
      (doseq [ghost (:ghosts @state/game-state)]
        (redraw-block (:old ghost))
        (swap! @state/game-state update-in [:ghost-pos] conj (ghost))
        (ghost/draw ghost (:ctx @state/game-state)))
      (user/draw-dead (:ctx @state/game-state) (/ (:tick @state/game-state) (* const/FPS 2))))))

(defn game-countdown []
  (let [diff (Math/floor (/ (- (:timer-start @state/game-state) (:tick @state/game-state)) (const/FPS)))]
    (if (== diff 0)
      (do 
        (gamemap/draw (:ctx @state/game-state))
        (set-state (:playing const/game-const)))
      (if-not (= diff (:last-time @state/game-state))
        (do
          (swap! state/game-state assoc-in [:last-time] diff)
          (gamemap/draw (:ctx @state/game-state))
          (dialog (str "Starting in: " diff) (:ctx @state/game-stae)))))))

(defn main-loop []
  ;; (if (not= (:state @state/game-state) (:pause const/game-const)) 
  
  ;; This doesn't work RIGHT NOW!
  ;;   (swap! state/game-state update-in [:tick] (fnil inc 0)))
  (gamemap/draw-pills (:ctx @state/game-state))
  (let [state (:state @state/game-state)]
    (cond 
     (= state (:playing const/game-const)) (main-draw)
     (and (= state (:waiting const/game-const)) (:state-changed @state/game-state)) (start-new-game) 
     (and (= state (:eaten-pause const/game-const)) (> (- (:tick @state/game-state) (:timeser-start @state/game-state)) (* const/FPS 2))) (game-playing)       
     (= (:state @state/game-state) (:dying (const/game-const))) (game-dying)
     (= (:state @state/game-state) (:countdown const/game-const)) (game-countdown)))
   (draw-footer))

(defn eaten-pill []
  (audio/play "eatpill")
  (swap! state/game-state assoc-in [:timer-start] (:tick @state/game-state))
  (swap! state/game-state assoc-in [:eaten-count] 0)
  (doseq [ghost (:ghosts @state/game-state)]
     (ghost/make-eatable ghost)))

(defn completed-level [])

(defn key-press [e])

(defn loaded []
  (dialog "Press N to Start" (:ctx @state/game-state))
  (.setInterval js/window (main-loop) (/ 1000 const/FPS)))

(defn load [audio-files callback]
  (if (= (count audio-files) 0) (callback)
      (doseq [file audio-files]
        (let [[name path] audio-files]
          (audio/load name path (load audio-files (callback)))))))

(defn init [wrapper root]
  (let [canvas (.createElement js/document "canvas")
        block-size (/ (.-offsetWidth wrapper) 19)]
    (.setAttribute canvas "width" (str (* block-size 19) "px"))
    (.setAttribute canvas "height" (str (+ (* block-size 22) 30) "px"))
    (.appendChild wrapper canvas)

    ;; Set mutable vars
    (swap! state/game-state assoc-in [:ctx] (.getContext canvas "2d"))
    (swap! gamemap/map-state assoc-in [:block-size] block-size)
    (swap! state/game-state update-in [:audio] conj {:sound-disabled true})
    (swap! state/game-state update-in [:user] conj {:completed-level completed-level
                                                    :eaten-pill eaten-pill})

    (doseq [specs (:ghost-specs @state/game-state)]
        (swap! state/game-state update-in [:ghosts] conj {:get-tick (helper/get-tick) 
                                                          :map (:map @state/game-state) 
                                                          :specs specs
                                                          :eatable nil
                                                          :due nil
                                                          :position nil
                                                          :direction nil
                                                          :eaten nil
                                                          :color nil}))

    (helper/console-log (:ghosts @state/game-state))
    (gamemap/draw (:ctx @state/game-state)) 
    (dialog "Loading..." (:ctx @state/game-state))
    (let [extension (str "mp3")
          audio-files [{:start (str root "audio/opening_song." extension)}
                       {:die (str root  "audio/die." extension)}
                       {:eatghost (str root "audio/eatghost." extension)}
                       {:eatpill (str root "audio/eatpill." extension)}
                       {:eating (str root  "audio/eating.short." extension)}
                       {:eating2 (str root "audio/eating.short." extension)}]]
      (load audio-files loaded))))

;; Init!
(def elem (helper/get-element-by-id "pacman"))
(.setTimeout js/window (fn [x] (init elem "./")) 0)
;(set! (.-onload js/window) (init elem "./"))