(ns pacman.core
  (:require [pacman.constants :as const]
            [pacman.ghost :as ghost]
            [pacman.audio :as audio]
            [pacman.gamemap :as gamemap]
            [pacman.helpers :as helper]
            [pacman.state :as state]
            [goog.dom :as dom]
            [clojure.browser.repl :as repl]))

(repl/connect "http://localhost:9000/repl")
;; App
;; ------------------------------------------------------------------------------------------------------

(defn draw-score 
  [text position]
  (let [ctx (:ctx @state/game-state)]
    (set! (. ctx  -fillStyle) "#FFFFFF")
    (set! (. ctx -font) "12px BDCartoonShoutRegular")
    (.fillText ctx text (* 10 (:block-size gamemap/map-state)) (* 10 (:block-size gamemap/map-state)))))

(defn dialog [text ctx]
  (set! (. ctx  -fillStyle) "#FFFF00")
  (set! (. ctx -font) "14px BDCartoonShoutRegular")
  (let [dialog-width (.-width (.measureText ctx text))
        map-width (alength (aget const/game-map 0))
        map-height (alength const/game-map)
        x (/ (- (* map-width (:block-size gamemap/map-state)) dialog-width) 2)]
    (.fillText ctx text x (+ (* map-height 10) 8))))

(defn start-level [state]
  (-> state
    (assoc :timer-start (:tick @state/game-state))
    (assoc :state (:countdown const/game-const))))

(defn start-new-game [state]
  (-> state
    (assoc :state (:const const/game-const))
    (assoc :level 1)
    (start-level)))

(defn sound-disabled? []
  (.setItem (.-localStorage (dom/getWindow)) "sound-disabled" true))

(defn key-down [state e]
  (let [kc (.-keyCode e)]
    (condp = kc 
      (:N const/KEYS) (start-new-game state)
      (:S const/KEYS) (.setItem (.-localStorage (dom/getWindow)) "sound-disabled" false)
      (:P const/KEYS) (-> state
                        (draw) 
                        (assoc :satet (:stored state)))
      (:P const/KEYS) (-> state
                        (assoc :stored (:state state))
                        (assoc :state (:pause const/game-const))
                        (draw)
                        (dialog "Paused" (:ctx @state/game-state)))
      :else (if (and kc (not= (:state state) (:pause const/game-const)))
              (do
                (.preventDefault e)
                (.stopPropagation e)
                (assoc-in state [:user :due] (get key-map key-code)))
              state))))

(defn lose-life [])

(defn collided [user ghost]
  (< (+ (Math/sqrt (Math/pow (- (:x ghost) (:x user)) 2) 
                   (Math/pow (- (:y ghost) (:y user)) 2))) 10))

(defn draw-footer [ctx]
  (let [block-size (:block-size gamemap/map-state)
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

    ;; TODO: think about how to implement sound-disabled ...
    (set! (. ctx -fillStyle) (if (sound-disabled?) "#00FF00" "#FF0000"))
    (set! (. ctx -font) "bold 16px sans-serif")
    (.fillText ctx "s" 10 text-base)
    
    (set! (. ctx -fillStyle) "#FFFF00")
    (set! (. ctx -font) "14px BDCartoonShoutRegular")
    
    (.fillText ctx (str "Score: " (user/the-score)) 30 text-base)
    (.fillText ctx (str "Level: " (:level @state/game-state)) 260 text-base)))

(defn redraw-block [pos]
  (let [ctx (:ctx @state/game-state)]
    (gamemap/draw-block (Math/floor (/ (:y pos) 10)) (Math/floor (/ (:x pos) 10)) (:block-size gamemap/map-state) ctx)
    (gamemap/draw-block (Math/ceil (/ (:y pos) 10)) (Math/ceil (/ (:x pos) 10)) (:block-size gamemap/map-state) ctx)))

(defn main-draw []
  
  (user/move @user/user-state)
  (redraw-block (:old-pos @user/user-state))
  ;;(.log js/console (pr-str @user/user-state))
  (user/draw)
  ;(swap! user/user-state (user/move @user/user-state))
  ;
  ;
  
  #_(doseq [g (:ghosts @state/game-state)]
    (ghost/draw g))

  ;; loop 4
  #_(doseq [ghost (:ghosts @state/game-state)]
    (if (collided (:position @user/user-state) (let ([pos] ())))
      (if (ghost/is-vulnerable? ghost)
                                        ;(audio/play "eatghost")
        (ghost/eat ghost)
        (swap! state/game-state update-in [:eaten-count] (fnil inc 0))
        (swap! state/game-state assoc-in [:n-score] (* (:eaten-count @state/game-state) 50))
        (user/add-score (:n-score @state/game-state))
        (helper/set-state (:eaten-pause const/game-const))
        (swap! state/game-state assoc-in [:timer-start] (:tick @state/game-state)))
      (if (ghost/is-dangerous? ghost)
                                        ;(audio/play "die")
        (helper/set-state (:dying const/game-const))
        (swap! state/game-state assoc-in [:timer-start] (:tick @state/game-state))))))

(defn start-game []
  (swap! state/game-state assoc-in [:state-changed] false)
  (gamemap/draw (:ctx @state/game-state))
  (dialog "Press N to start a New game" (:ctx @state/game-state)))

(defn game-playing []
  (gamemap/draw (:ctx @state/game-state)) 
  (helper/set-state (:playing const/game-const)))

(defn game-dying []
  (if (> (- (:tick (@state/game-state)) (:timer-start @state/game-state)) (/ (const/FPS) 3))     
    (lose-life)
    (do
      (redraw-block (:position @user/user-state))
      (doseq [ghost (:ghosts @state/game-state)]
        (redraw-block (:old ghost))
        (swap! @state/game-state update-in [:ghost-pos] conj (ghost))
        (ghost/draw ghost (:ctx @state/game-state)))
      (user/draw-dead (:ctx @state/game-state) (/ (:tick @state/game-state) (* const/FPS 2))))))

(defn game-countdown []
  (let [diff (+ 1 (.floor js/Math (/ (- (:timer-start @state/game-state) (:tick @state/game-state)) const/FPS)))
        ]
    (if (= diff 0)
      (do 
        (gamemap/draw (:ctx @state/game-state))
        (swap! state/game-state assoc-in [:state] (:playing const/game-const)))
      (if-not (= diff (:last-time @state/game-state))
        (do
          (swap! state/game-state assoc-in [:last-time] diff)
          (gamemap/draw (:ctx @state/game-state))
          (dialog (str "Starting in: " diff) (:ctx @state/game-state)))))))

(defn main-loop []
  (if-not (= (:state @state/game-state) (:pause const/game-const)) 
    (swap! state/game-state update-in [:tick] (fnil inc 0)))

  (let [state (:state @state/game-state)
        ctx (:ctx @state/game-state)]

    (gamemap/draw-pills ctx)

    (cond 
     (= state (:playing const/game-const)) (main-draw)
     (and (= state (:waiting const/game-const)) (:state-changed @state/game-state)) (start-game) 
     (and (= state (:eaten-pause const/game-const)) 
          (> (- (:tick @state/game-state) (:timer-start @state/game-state)) (* const/FPS 2))) (game-playing)       
     (= (:state @state/game-state) (:dying const/game-const)) (game-dying)
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
  (helper/set-interval (/ 1000 const/FPS) #(main-loop)))

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
