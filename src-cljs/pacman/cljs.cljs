(ns pacman.core
  (:require [pacman.constants :as const]
            [pacman.ghost :as ghost]
            [pacman.user :as user]
            [pacman.audio :as audio]
            [pacman.gamemap :as gamemap]
            [pacman.helpers :as helper]
            [pacman.state :as state]
            [goog.dom :as dom]))

;;------------------------------------------------------------------------------------------ 
;; App

(defn draw-score 
  [text position]
  (let [ctx (:ctx @state/game-state)]
    (set! (. ctx  -fillStyle) "#FFFFFF")
    (set! (. ctx -font) "12px BDCartoonShoutRegular")
    (.fillText ctx text (* 10 (:block-size @state/game-state)) (* 10 (:block-size @state/game-state)))))

(defn dialog [text ctx]
  (set! (. ctx  -fillStyle) "#FFFF00")
  (set! (. ctx -font) "14px BDCartoonShoutRegular")
  (let [dialog-width (.-width (.measureText ctx text))
        map-width (alength (aget const/game-map 0))
        map-height (alength const/game-map)
        x (/ (- (* map-width (:block-size @gamemap/map-state)) dialog-width) 2)]
    (.fillText ctx text x (+ (* map-height 10) 8))))

(defn set-state [n-state]
  (swap! state/game-state assoc-in [:state] n-state)
  (swap! state/game-state assoc-in [:state-changed] true))

(defn sound-disabled []
  (.setItem (.-localStorage (dom/getWindow)) "sound-disabled" true))

(defn start-level []
  (user/reset-position)
  (ghost/reset-ghost-state! (:ghosts @state/game-state))

  ;(audio/play "start")
  ;(swap! state/game-state assoc-in [:timer-start] (:tick @state/game-state))
  ;(set-state (:countdown const/game-const))
)

(helper/console-log (:ghosts @state/game-state))

(defn start-new-game []
  (set-state (:const const/game-const))
  (swap! state/game-state assoc-in [:level] 1)
  (user/reset)
  (gamemap/reset)
  (gamemap/draw (:ctx @state/game-state))
  (start-level)
)

(defn key-down [e]
  ;(helper/console-log (= (.-keyCode e) (:N (:keys @const/KEY))))
  (cond 
   (= (.-keyCode e) (:N (:keys @const/KEY))) (start-new-game)

   ;; (= (.-keyCode e) (:S @const/KEY))(do (audio/disable-sound)   
   ;;                                     (.setItem (.-localStorage (dom/getWindow)) "sound-disabled" false))
   ;; (and (= (.-keyCode e) (:P (:keys @const/KEY))) (= (:state @state/game-state) (:pause const/game-const))) (do (audio/resume) 
   ;;                                                                                                             (gamemap/draw (:ctx @state/game-state)) 
   ;;                                                                                                             (set-state (:stored @state/game-state)))
   ;; (= (.-keyCode e) (:P (:keys @const/KEY))) (user/keydown e))
))

(defn lose-life [])

(defn collided [user ghost]
  (< (+ (Math/sqrt (Math/pow (- (:x ghost) (:x user)) 2) 
                   (Math/pow (- (:y ghost) (:y user)) 2))) 10))

(defn draw-footer [ctx]
  (let [block-size (:block-size @gamemap/map-state)
        map-width (alength (aget const/game-map 0))
        map-height (alength const/game-map)
        top-left (* map-height block-size)
        text-base (+ top-left 17)]

    (set! (. ctx -fillStyle) "#000000")
    (.fillRect ctx 0 top-left (* map-width block-size) 30)
    (set! (. ctx -fillStyle) "#FFFF00")

    (doseq [i (range (user/get-lives))]
      (set! (. ctx -fillStyle) "#FFFF00")
      (.beginPath ctx)
      (.moveTo ctx (+ 150 (* 25 i) (/ block-size 2)) 
                   (+ (+ top-left 1) (/ block-size 2)))

      (.arc ctx (+ (+ 150 (* 25 i)) (/ block-size 2)) 
                (+ (+ top-left 1) (/ block-size 2)) 
                (/ block-size 2)
                (* (.-PI js/Math) 0.25)
                (* (.-PI js/Math) 1.75)
                false)
      (.fill ctx))

    (set! (. ctx -fillStyle) (if (sound-disabled) "#00FF00" "#FF0000"))
    (set! (. ctx -font) "bold 16px sans-serif")
    (.fillText ctx "s" 10 text-base)
    
    (set! (. ctx -fillStyle) "#FFFF00")
    (set! (. ctx -font) "14px BDCartoonShoutRegular")
    
    (.fillText ctx (str "Score: " (user/the-score)) 30 text-base)
    (.fillText ctx (str "Level: " (:level @state/game-state)) 260 text-base)))

(defn redraw-block [pos ctx]
  (gamemap/draw-block (Math/floor (/ (:y pos) 10)) (Math/floor (/ :x pos) 10) ctx)
  (gamemap/draw-block (Math/ciel (/ (:y pos) 10)) (Math/ceil (/ :x pos) 10) ctx))

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
    (redraw-block (:old (:ghost-pos @state/game-state)) (:ctx @state/game-state)))
  
  ;; this may not be the right param for redraw-block
  (redraw-block (:old (:user @state/game-state)) (:ctx @state/game-state))

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

(defn start-game []
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
      (redraw-block (:user-pos @state/game-state) (:ctx @state/game-state))
      (doseq [ghost (:ghosts @state/game-state)]
        (redraw-block (:old ghost) (:cts @state/game-state))
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
  (if (not= (:state @state/game-state) (:pause const/game-const)) 
    (swap! state/game-state update-in [:tick] (fnil inc 0)))

  (gamemap/draw-pills (:ctx @state/game-state))

  (let [state (:state @state/game-state)]
    (cond 
     (= state (:playing const/game-const)) (main-draw)
     (and (= state (:waiting const/game-const)) (:state-changed @state/game-state)) (start-game) 
     (and (= state (:eaten-pause const/game-const)) (> (- (:tick @state/game-state) (:timeser-start @state/game-state)) (* const/FPS 2))) (game-playing)       
     (= (:state @state/game-state) (:dying (const/game-const))) (game-dying)
     (= (:state @state/game-state) (:countdown const/game-const)) (game-countdown)))
  (draw-footer (:ctx @state/game-state)))

(defn eaten-pill []
  (audio/play "eatpill")
  (swap! state/game-state assoc-in [:timer-start] (:tick @state/game-state))
  (swap! state/game-state assoc-in [:eaten-count] 0)
  (doseq [ghost (:ghosts @state/game-state)]
    (ghost/make-eatable ghost)))

(defn completed-level [])

(defn key-press [e]
  (if (and (not= (:state @state/game-state) (:waiting const/game-const)) (not= (:state @state/game-state) (:pause const/game-const)))
    (.preventDefault e)
    (.stopPropagation e)))

(defn loaded []
  (dialog "Press N to Start" (:ctx @state/game-state))
  (.addEventListener js/document "keydown" key-down true)
  (.addEventListener js/document "keypress" key-press true)
  (swap! state/game-state assoc-in [:timer] (.setInterval js/window (main-loop) (/ 1000 const/FPS))))

(defn load [audio-files callback]
  (if (= (count audio-files) 0) 
    (callback)
    (doseq [file audio-files]
      (let [{name :name path :path} file]
        ;(audio/load name path #(load audio-files callback))
))))

(defn init [wrapper root]
  (let [canvas (.createElement js/document "canvas")
        block-size (/ (.-offsetWidth wrapper) 19)]

    (.setAttribute canvas "width" (str (* block-size 19) "px"))
    (.setAttribute canvas "height" (str (+ (* block-size 22) 30) "px"))
    (.appendChild wrapper canvas)

    (swap! state/game-state assoc-in [:ctx] (.getContext canvas "2d"))
    (swap! gamemap/map-state assoc-in [:block-size] block-size)
    (swap! state/game-state update-in [:audio] conj {:sound-disabled true})
    (swap! state/game-state update-in [:user] conj {:completed-level completed-level
                                                    :eaten-pill eaten-pill})
    (gamemap/draw (:ctx @state/game-state)) 
    (dialog "Loading..." (:ctx @state/game-state))
    (let [extension (str "mp3")
          audio-files [{:name "start" :path (str root "audio/opening_song." extension)}
                       {:name "die"   :path (str root  "audio/die." extension)}
                       {:name "eatghost" :path (str root "audio/eatghost." extension)}
                       {:name "eatpill"  :path (str root "audio/eatpill." extension)}
                       {:name "eating"  :path (str root  "audio/eating.short." extension)}
                       {:name "eating2" :path (str root "audio/eating.short." extension)}]]
      (load audio-files (loaded)))))

;; Init!
(def elem (helper/get-element-by-id "pacman"))
(.setTimeout js/window (fn [x] (init elem "./")) 0)
