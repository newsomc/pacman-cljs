(ns pacman.game
;  (:require-macros [pacman.macros :as m])
  (:require [pacman.constants :as const]
            [pacman.helpers :as helper]
            [goog.dom :as dom]
            [clojure.browser.repl :as repl]))

(repl/connect "http://localhost:9000/repl")

;; =============================================================================
;; Definitions

(def canvas (.getElementById js/document "canvas"))
(def ctx (.getContext canvas "2d"))
(def controls { (:ARROW_LEFT  const/KEYS) const/LEFT
                (:ARROW_UP    const/KEYS) const/UP
                (:ARROW_RIGHT const/KEYS) const/RIGHT
                (:ARROW_DOWN  const/KEYS) const/DOWN })

;; Game State
(defn make-ghost [color] 
  {:get-tick 0, 
   :eatable nil, 
   :color nil, 
   :eaten nil, 
   :specs color, 
   :position nil, 
   :due nil, 
   :map nil, 
   :npos nil,
   :old-pos nil,
   :direction nil})

(def ghost-specs ["#00FFDE" "#FF0000" "#FFB8DE" "#FFB847"])

(def game-state
  {:phase :waiting
   :dialog nil
   :countdown 4
   :user {:position nil
          :direction const/LEFT
          :eaten 0
          :due const/LEFT
          :lives 0
          :score 5
          :npos nil
          :next-whole nil
          :block nil
          :old-pos nil}
   :map {:height nil
         :width nil
         :pill-size 0 
         :block-size nil
         :board const/game-map}
   :audio []
   :ghosts (mapv make-ghost ghost-specs)
   :ghost-specs ["#00FFDE" "#FF0000" "#FFB8DE" "#FFB847"]
   :eaten-count 0
   :level 0
   :tick 0
   :ghost-pos []
   :state-changed true
   :timer-start nil
   :last-time 0
   :ctx nil
   :timer nil
   :stored nil
   :n-score 0})

;; =============================================================================
;; Draw Functions

(defn draw-dialog [{map :map dialog :dialog :as state}]
  (if dialog
    (do
      (set! (. ctx  -fillStyle) "#FFFF00")
      (set! (. ctx -font) "14px BDCartoonShoutRegular")
      (let [dialog-width (.-width (.measureText ctx dialog))
            map-width (alength (aget const/game-map 0))
            map-height (alength const/game-map)
            x (/ (- (* map-width (:block-size map)) dialog-width) 2)]
        (.fillText ctx dialog x (+ (* map-height 10) 8)) 
        state)))
  state)

(defn draw-score 
  [{map :map :as state} text position]
  (set! (. ctx  -fillStyle) "#FFFFFF")
  (set! (. ctx -font) "12px BDCartoonShoutRegular")
  (.fillText ctx text (* 10 (:block-size map)) (* 10 (:block-size map)))
  state)

(declare draw-block draw-pills)

(defn draw-footer [{map :map user :user :as state}]
  (let [block-size (:block-size map)
        map-width (alength (aget const/game-map 0))
        map-height (alength const/game-map)
        top-left (* map-height block-size)
        text-base (+ top-left 17)]
    (set! (. ctx -fillStyle) "#000000")
    (.fillRect ctx 0 top-left (* map-width block-size) 30)
    (set! (. ctx -fillStyle) "#FFFF00")
    (doseq [i (range (:lives user))]
      (set! (. ctx -fillStyle) "#FFFF00")
      (doto ctx
        (.beginPath)
        (.moveTo (+ 150 (* 25 i) (/ block-size 2)) 
                 (+ (+ top-left 1) (/ block-size 2)))
        (.arc (+ (+ 150 (* 25 i)) (/ block-size 2)) 
              (+ (+ top-left 1) (/ block-size 2)) 
              (/ block-size 2)
              (* (.-PI js/Math) 0.25)
              (* (.-PI js/Math) 1.75)
              false)
        (.fill)
        state))

    (set! (. ctx -font) "bold 16px sans-serif")
    (.fillText ctx "s" 10 text-base)
    
    (set! (. ctx -fillStyle) "#FFFF00")
    (set! (. ctx -font) "14px BDCartoonShoutRegular")
    
    (.fillText ctx (str "Score: " (:score user)) 30 text-base)
    (.fillText ctx (str "Level: " (:level state)) 260 text-base)
    state))

;; =============================================================================
;; Draw Pac-Man

(defn calc-angle [dir pos]
  (cond
    (and (= dir const/RIGHT) (< (mod (:x pos) 10) 5)) {:start 0.25 :end 1.75 :direction false}
    (and (= dir const/DOWN)  (< (mod (:y pos) 10) 5)) {:start 0.75 :end 2.25 :direction false}
    (and (= dir const/UP)    (< (mod (:y pos) 10) 5)) {:start 1.25 :end 1.75 :direction true}     
    (and (= dir const/LEFT)  (< (mod (:x pos) 10) 5)) {:start 0.75 :end 1.25 :direction true}
    :else {:start 0 :end 2 :direction false}))

;; state - no longer returns state. Instead, return redraw-block.
(defn redraw-block [{map :map :as state}]
  (let [{{pos :position} :user} state
        bs (:block-size map)
        by (Math/floor (/ (:y pos) 10))
        bx (Math/floor (/ (:x pos) 10))]
    (draw-block state by bx bs)))

(defn draw-pacman [{map :map user :user :as state}]
  (helper/console-log user)
  (let [s        (:block-size map)
        position (:npos user)
        angle    (calc-angle (:direction user) position)]
    (set! (. ctx  -fillStyle) "#FFFF00")
    (doto ctx
      (.beginPath)
      (.moveTo (+ (* (/ (:x position) 10) s) (/ s 2))
        (+ (* (/ (:y position) 10) s) (/ s 2)))
      (.arc (+ (* (/ (:x position) 10) s) (/ s 2))
        (+ (* (/ (:y position) 10) s) (/ s 2))
        (/ s 2)
        (* (.-PI js/Math) (:start angle))
        (* (.-PI js/Math) (:end angle))
        (:direction angle))
      (.fill))
    state))

(defn draw-dead [{map :map :as state} amount]
  (let [size (:block-size map)
        half (/ size 2)
        position (:position (:user state))]
    (if-not (>= amount 1)
      (set! (. ctx  -fillStyle) "#FFFF00")
      (doto ctx
        (.beginPath)
        (.moveTo (+ (* (/ (:x position) 10) size) half)
          (+ (* (/ (:y position) 10) size) half))

        (.arc (+ (* (/ (:x position) 10) size) half)
          (+ (* (/ (:y position) 10) size) half)
          half
          0
          (* (.-PI js/Math) 2 amount)
          true)
        (.fill)))
    state))

(declare draw-wall draw-block is-floor-space? move-pacman)

(defn draw-map
  "Main draw function for game board."
  [{map :map :as state}]
  (set! (. ctx  -fillStyle) "#000")
  (let [width  (:width map)
        height (:height map)
        size   (:block-size map)]
    (.fillRect ctx 0 0 (* width size) (* height size))
    (draw-wall map)
    (doseq [i (range height)] 
      (doseq [j (range width)]
        (draw-block state i j size)))
    state))

(defn main-draw [state]
  (let [new-state (-> state
                    (draw-map)
                    (draw-footer)
                    (draw-pills)
                    (draw-dialog))]
    (if (= :playing (:phase state))
      (do (-> new-state
            (redraw-block)
            (move-pacman)
            (draw-pacman))
        new-state)
      state)))

;; =============================================================================
;; Event Handlers

(def keydown-state (atom nil))

(defn handle-keydown [e]
    (reset! keydown-state (.-keyCode e))
    (.preventDefault e)
    (.stopPropagation e))

;; =============================================================================
;; Update Functions & Helpers

(defn start-level [state]
  (-> state
    (assoc :timer-start (- (:tick state) 1))
    (assoc :phase :countdown)))

(defn start-new-game [state]
  (-> state
    (assoc :level 1)
    (start-level)))

(defn resume-game [state]
  (-> state 
    (assoc :dialog nil)
    (assoc :phase :playing)))

(defn pause-game [state]
    (-> state
      (assoc :dialog "Paused")
      (assoc :phase :pause)))

(defn toggle-pause [state]
  (if (= (:phase state) :pause)
    (resume-game state)
    (pause-game state)))

(defn keydown [state]
  (let [kc @keydown-state]
    (reset! keydown-state nil)
    (condp = kc
      (:N const/KEYS) (start-new-game state)
      (:S const/KEYS) (.setItem (.-localStorage (dom/getWindow)) "sound-disabled" false)
      (:P const/KEYS) (toggle-pause state)
      (if (and kc (not= (:phase state) :pause))
        (do 
          (assoc-in state [:user :due] (get controls kc)))
        state))))

(defn lose-life [state])

(defn collided? [user ghost]
  (< (+ (Math/sqrt (Math/pow (- (:x ghost) (:x user)) 2) 
                   (Math/pow (- (:y ghost) (:y user)) 2))) 10))

(defn is-on-same-plane? [due dir]
  (or (and (or (= due const/LEFT) (= due const/RIGHT)) 
           (or (= dir const/LEFT) (= dir const/RIGHT)))
      (and (or (= due const/UP)   (= due const/DOWN)) 
           (or (= dir const/UP)   (= dir const/DOWN)))))

(defn on-whole-square? [p]
  (= (mod p 10) 0))

(defn on-grid-square? [pos]
  (and (on-whole-square? (:y pos)) (on-whole-square? (:x pos))))

(defn point-to-coord [n]
  (Math/round (/ n 10)))

(defn next-square [n dir]
  (let [rem (mod n 10)]
    (cond
      (= rem 0) n
      (or (= dir const/RIGHT) (= dir const/DOWN)) (+ n (- 10 rem))
      :else (- n rem))))

(defn next-pos [pos dir]
  {:y (point-to-coord (next-square (:y pos) dir))
   :x (point-to-coord (next-square (:x pos) dir))})

(defn is-mid-square? [x]
  (let [rem (mod x 10)]
    (or (> rem 3) (< rem 7))))

;; Also used for moving Pac-Man. Determines if he is facing a wall.
(defn direction-allowable? [map due dir pos npos]
  (and (or (is-on-same-plane? due dir) 
           (on-grid-square? pos)) 
    (is-floor-space? map (next-pos npos due))))

(defn map-pos [y x]
  (aget const/game-map y x))

(defn within-bounds? [map x y]
  (and (>= y 0) (< y (:height map)) (>= x 0) (< x (:width map))))

(defn is-wall-space? [map pos]
  (and (within-bounds? map (:x pos) (:y pos)) 
       (= const/WALL (map-pos (:y pos) (:x pos)))))

(defn is-floor-space? [map pos]
  (if (within-bounds? map (:x pos) (:y pos))
    (let [piece (map-pos (:y pos) (:x pos))]
      (or (= piece const/EMPTY)
          (= piece const/BISCUIT)
          (= piece const/PILL)))))

(defn draw-wall [map]
  (set! (. ctx -strokeStyle) "#0000FF")
  (set! (. ctx -lineWidth) 5)
  (set! (. ctx -lineCap) "round")
  (letfn [(*block-size [n] (* n (:block-size map)))]
    (doseq [line const/WALLS]
      (.beginPath ctx)
      (doseq [point line]
        (cond (:move point) (let [[a b] (:move point)]
                              (.moveTo ctx (*block-size a) (*block-size b)))
          (:line point) (let [[a b] (:line point)] 
                          (.lineTo ctx (*block-size a) (*block-size b)))
          (:curve point) (let [[a b c d] (:curve point)] 
                           (.quadraticCurveTo ctx 
                             (*block-size a)
                             (*block-size b)
                             (*block-size c)
                             (*block-size d)))))       
      (.stroke ctx))))

(defn block [pos]
  (map-pos (:y pos) (:x pos)))

(defn draw-pills [{map :map :as state}]
  (let [height     (:height map)
        width      (:width map)
        block-size (:block-size map)]
    (doseq [i (range height)]
      (doseq [j (range width)]
        (if (= (map-pos i j) const/PILL)
          (do
            (.beginPath ctx)
            (set! (. ctx -fillStyle) "#000")
            (.fillRect ctx (* j block-size) 
                           (* i block-size)
                           block-size 
                           block-size)
            (set! (. ctx -fillStyle) "#FFF")
            (.arc ctx (+ (* j block-size) (/ block-size 2))
                      (+ (* i block-size) (/ block-size 2))
                     (Math/abs (- 5 (/ (:pill-size map) 3)))
                      0
                      (* (.-PI js/Math) 2)
                      false)
            (.fill ctx)
            (.closePath ctx)))))
    state))

(defn draw-biscuit [y x layout {map :map :as state}]
  (let [block-size (:block-size map)]
    (set! (. ctx -fillStyle) "#000")
    (.fillRect ctx (* x block-size) (* y block-size) block-size block-size)
    (if (= layout const/BISCUIT)
      (do
        (set! (. ctx -fillStyle) "#FFF")
        (.fillRect ctx (+ (* x block-size) (/ block-size 2.5)) 
          (+ (* y block-size) (/ block-size 2.5))
          (/ block-size 6)
          (/ block-size 6))))
    state))

(defn draw-block [state y x block-size] 
  (let [layout (map-pos y x)]
    (if-not (= layout const/PILL) 
      (do   
        (.beginPath ctx)
        (if (or (= layout const/EMPTY)
                (= layout const/BLOCK)
                (= layout const/BISCUIT)) 
          (draw-biscuit y x layout state))
        (.closePath ctx)))
    state))

;; ============================================================================================
;; Move Pac-Man

(defn get-new-coord [dir pos]
  (let [x (or (and (= dir const/LEFT) -2)
              (and (= dir const/RIGHT) 2) 
              0)
        y (or (and (= dir const/DOWN) 2)
              (and (= dir const/UP) -2) 
              0)]
    {:x (+ (:x pos) x)
     :y (+ (:y pos) y)}))

(defn get-new-npos [due dir pos npos]
  (cond 
    (and (= (:y npos) 100) (>= (:x npos) 190) (= dir const/RIGHT)) {:y 100 :x -10}
    (and (= (:y npos) 100) (<= (:x npos) -12) (= dir const/LEFT))  {:y 100 :x 190}
    (= dir const/FACINGWALL) pos
    :else (if-not (= due dir) 
            (get-new-coord due pos)
            (get-new-coord dir pos))))

(defn facing-wall? [map pos npos dir]
  (and (on-grid-square? pos) (is-wall-space? map (next-pos npos dir))))

(defn get-new-direction [map due dir pos npos]
  (cond 
    (facing-wall? map pos npos dir) const/FACINGWALL
    (and (not= due dir) (direction-allowable? map due dir pos npos)) due
    :else due))

;; (defn move-pacman [{user :user map :map :as state}] 
;;   (let [{due :due dir :direction
;;          pos :position npos :npos} user]
;;     (if (= (:phase state) :playing)
;;       (-> state
;;         (assoc-in [:user :npos] (get-new-npos due dir pos npos))
;;         (assoc-in [:user :position] npos)
;;         (assoc-in [:user :old-pos] pos)
;;         (assoc-in [:user :direction] (get-new-direction map due dir pos npos))
;;         (assoc-in [:user :due] dir)))))

(defn refresh-user-data [{user :user map :map :as state}] 
  (let [{due :due dir :direction
         pos :position npos :npos} user]    
    (if (= (:phase state) :playing)
      { :npos      (get-new-npos due dir pos npos) 
        :position  npos
        :old-pos   pos
        :direction (get-new-direction map due dir pos npos)
        :due       dir}
      user)))

(defn move-pacman [state]
  (update-in state [:user]
    (fn [user]
      (merge user (refresh-user-data state)))))

;; ============================================================================================
;; Game Phases

(defn start-game [state]
  (-> state
    (assoc :state-changed false)))

(defn game-playing [state]
  (-> state
    (assoc :phase :playing)))

(defn game-dying [state]
  (if (> (- (:tick state) (:timer-start state)) (/ (const/FPS) 3))     
    (lose-life state)
    (-> state
      (redraw-block)
      (draw-dead (/ (:tick state) (* const/FPS 2))))))

(def ticks-remaining (atom 0))

(defn game-countdown [state]
  (if (zero? @ticks-remaining)
    (if (= (:countdown state) 1)
      (do
        (reset! ticks-remaining 0)
        (-> state game-playing (assoc :dialog nil) (assoc :countdown 4)))
      (do 
        (reset! ticks-remaining const/FPS)
        (-> state 
          (update-in [:countdown] dec) 
          (assoc :dialog (str "Starting in: " (dec (:countdown state)))))))
    (do
      (swap! ticks-remaining dec)
      state)))
 
(defn eaten-pill [state]
  (-> state
    (assoc :timer-start (:tick state))
    (assoc :eaten-count 0)))

;; ============================================================================================
;; Main Game Loop

(defn driver  [state]
  (let [phase (:phase state)
        state (-> (if (= phase :pause) 
                    state
                    (update-in state [:tick] (fnil inc 0)))
                (keydown)
                ;(update-in [:user] pacman-move)
)]
    (main-draw
      (cond 
        (= phase :playing) state;(game-playing state)
        (and (= phase :waiting) (:state-changed state)) (start-game state) 
        (and (= phase :eaten-pause) 
          (> (- (:tick state) (:timer-start state)) (* const/FPS 2))) (game-playing state)
        (= phase :dying) (game-dying state)
        (= phase :countdown) (game-countdown state)
        :else state))))

(defn make-state []
  (-> game-state
    (assoc :dialog "Press N to start a new game")
    (assoc-in [:user :position] {:x 90 :y 120})
    (assoc-in [:user :npos] {:x 90 :y 120})
    (assoc-in [:map :width] 19)
    (assoc-in [:map :height] 22)
    (assoc-in [:map :block-size] 18)))

(defn loaded []
  (let [init-state (make-state)
        interval   (/ 1000 const/FPS)]
    (.addEventListener js/document "keydown" handle-keydown true)
    (.setTimeout js/window
      (fn game-loop [s]
        (let [state (or s init-state)
              new-state (driver state)]
          (.setTimeout js/window
            #(game-loop new-state)
            interval)))
      interval)))

(defn init [wrapper root]
  (let [block-size (/ (.-offsetWidth wrapper) 19)]
    (.setAttribute canvas "width" (str (* block-size 19) "px"))
    (.setAttribute canvas "height" (str (+ (* block-size 22) 30) "px"))
    (.appendChild wrapper canvas)
    ;; a bit trickier maybe handle as special? - David
    ;;(dialog {} "Loading...")
    (loaded)))

;; Init!
(def elem (helper/get-element-by-id "pacman"))
(.setTimeout js/window (fn [x] (init elem "./")) 0)
