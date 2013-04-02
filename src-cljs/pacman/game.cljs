(ns pacman.game
  (:require [pacman.constants :as const]
            [pacman.helpers :as helper]
            [clojure.browser.repl :as repl]))

(repl/connect "http://localhost:9000/repl")

;; =============================================================================
;; Definitions

(def canvas (.getElementById js/document "canvas"))
(def ctx (.getContext canvas "2d"))

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

(def key-map {(:ARROW_LEFT  const/KEYS) (:left const/game-const)
              (:ARROW_UP    const/KEYS) (:up const/game-const)
              (:ARROW_RIGHT const/KEYS) (:right const/game-const)
              (:ARROW_DOWN  const/KEYS) (:down const/game-const)})

(def game-state
  {:phase :waiting
   :user {:position {:x 90 :y 120}
          :direction (:left const/game-const)
          :eaten 0
          :due (:left const/game-const)
          :lives 0
          :score 5
          :npos {:x 90 :y 120}
          :next-whole nil
          :block nil
          :old-pos nil}
   :map {:height nil
         :width nil
         :pill-size 0 
         :block-size nil
         :map const/game-map}
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
        (.fillText ctx dialog x (+ (* map-height 10) 8)))
      state)
    state))

(defn draw-score 
  [{map :map :as state} text position]
  (set! (. ctx  -fillStyle) "#FFFFFF")
  (set! (. ctx -font) "12px BDCartoonShoutRegular")
  (.fillText ctx text (* 10 (:block-size map)) (* 10 (:block-size map)))
  state)

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

    (set! (. ctx -font) "bold 16px sans-serif")
    (.fillText ctx "s" 10 text-base)
    
    (set! (. ctx -fillStyle) "#FFFF00")
    (set! (. ctx -font) "14px BDCartoonShoutRegular")
    
    (.fillText ctx (str "Score: " (:score user)) 30 text-base)
    (.fillText ctx (str "Level: " (:level state)) 260 text-base)
    state))

(defn calc-angle [dir pos]
  (cond
    (and (= dir (:right const/game-const)) (< (mod (:x pos) 10) 5)) {:start 0.25 :end 1.75 :direction false}
    (and (= dir (:down  const/game-const)) (< (mod (:y pos) 10) 5)) {:start 0.75 :end 2.25 :direction false}
    (and (= dir (:up const/game-const)) (< (mod (:y pos) 10) 5))    {:start 1.25 :end 1.75 :direction true}     
    (and (= dir (:left const/game-const)) (< (mod (:x pos) 10) 5))  {:start 0.75 :end 1.25 :direction true}
    :else {:start 0 :end 2 :direction false}))

(defn redraw-block [{map :map :as state}]
  (let [{{pos :pos} :user} state
        bs (:block-size map)
        by (Math/floor (/ (:y pos) 10))
        bx (Math/floor (/ (:x pos) 10))]
    (draw-block state by bx bs)
    (draw-block state by bx bs)
    state))

(defn draw-pacman [{map :map user :user :as state}]
  (let [s        (:block-size map)
        position (:position user)
        angle     (calc-angle (:direction user) position)]
    (set! (. ctx  -fillStyle) "#FFFF00")
    (.beginPath ctx)
    (.moveTo ctx (+ (* (/ (:x position) 10) s) (/ s 2))
                 (+ (* (/ (:y position) 10) s) (/ s 2)))
    (.arc ctx (+ (* (/ (:x position) 10) s) (/ s 2))
              (+ (* (/ (:y position) 10) s) (/ s 2))
              (/ s 2)
              (* (.-PI js/Math) (:start angle))
              (* (.-PI js/Math) (:end angle))
              (:direction angle))
    (.fill ctx)
    state))

(defn draw-dead [{map :map :as state} amount]
  (let [size (:block-size map)
        half (/ size 2)
        position (:position (:user state))]
    (if-not (>= amount 1)
      (do 
        (set! (. ctx  -fillStyle) "#FFFF00")
        (.beginPath ctx)
        (.moveTo ctx (+ (* (/ (:x position) 10) size) half)
                     (+ (* (/ (:y position) 10) size) half))

        (.arc ctx (+ (* (/ (:x position) 10) size) half)
                  (+ (* (/ (:y position) 10) size) half)
                  half
                  0
                  (* (.-PI js/Math) 2 amount)
                  true)
        (.fill ctx)))
    state))

(declare draw-wall draw-block is-floor-space?)

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
        (draw-block i j size ctx)))
    state))

(defn main-draw [state]
  (-> state
    (draw-map)
    ;;(draw-pills)
    ;; (draw-footer)
    ;; (redraw-block)
    (draw-pacman)
    ;; (draw-dialog)
    ))

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
    (assoc :timer-start (:tick state))
    (assoc :phase :countdown)))

(defn start-new-game [state]
  (-> state
    (assoc :phase :const)
    (assoc :level 1)
    (start-level)))

(defn keydown [state]
  (let [kc @keydown-state]
    (condp = kc 
      (:N const/KEYS) (start-new-game state)
      (:S const/KEYS) (.setItem (.-localStorage (dom/getWindow)) "sound-disabled" false)
      (:P const/KEYS) (-> state
                        (assoc :phase :stored))
      (:P const/KEYS) (-> state
                        (assoc :stored (:phase state))
                        (assoc :phase :pause)
                        (assoc :dialog "Paused"))
      (if (and kc (not= (:phase state) :pause))
        (assoc-in state [:user :due] (get key-map kc))
        state))))

(defn lose-life [state])

(defn collided? [user ghost]
  (< (+ (Math/sqrt (Math/pow (- (:x ghost) (:x user)) 2) 
                   (Math/pow (- (:y ghost) (:y user)) 2))) 10))

(defn is-on-same-plane? [due dir]
  (or (and (or (= due (:left const/game-const)) (= due (:right const/game-const))) 
           (or (= dir (:left const/game-const)) (= dir (:right const/game-const))))
      (and (or (= due (:up const/game-const)) (= due (:down const/game-const))) 
           (or (= dir (:up const/game-const)) (= dir (:down const/game-const))))))

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
      (or (= dir (:right const/game-const)) (= dir (:down const/game-const))) (+ n (- 10 rem))
      :else (- n rem))))

(defn next-pos [pos dir]
  {:y (point-to-coord (next-square (:y pos) dir))
   :x (point-to-coord (next-square (:x pos) dir))})

(defn is-mid-square? [x]
  (let [rem (mod x 10)]
    (or (> rem 3) (< rem 7))))

(defn direction-allowable? [map due dir pos npos]
  (and (or (is-on-same-plane? due dir) 
           (on-grid-square? pos)) 
    (is-floor-space? map (next-pos npos due))))

(defn map-pos [x y]
  (aget const/game-map x y))

(defn within-bounds? [map x y]
  (and (>= y 0) (< y (:height map)) (>= x 0) (< x (:width map))))

(defn is-wall-space? [map pos]
  (and (within-bounds? map (:x pos) (:y pos)) 
       (= const/WALL (map-pos (:y pos) (:x pos)))))

(defn is-floor-space? [map pos]
  (if (within-bounds? map (:x pos) (:y pos))
    (let [piece (map-pos (:x pos) (:y pos))]
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

(defn do-draw-biscuit [state y x layout block-size]
  (set! (. ctx -fillStyle) "#000")
  (.fillRect ctx (* x block-size) (* y block-size) block-size block-size)
  (if (= layout const/BISCUIT)
    (do
      (set! (. ctx -fillStyle) "#FFF")
      (.fillRect ctx (+ (* x block-size) (/ block-size 2.5)) 
                     (+ (* y block-size) (/ block-size 2.5))
                     (/ block-size 6)
                     (/ block-size 6))))
  state)

(defn draw-block [state y x block-size] 
  (let [layout (map-pos y x)]
    (if-not (= layout const/PILL) 
      (do   
        (.beginPath ctx)
        (if (or (= layout const/EMPTY)
                (= layout const/BLOCK)
                (= layout const/BISCUIT)) 
          (do-draw-biscuit state y x layout block-size ctx))
        (.closePath ctx)))
    state))

(defn get-new-coord [dir pos]
  (let [x (or (and (= dir (:left const/game-const)) -2)
              (and (= dir (:right const/game-const)) 2) 
              0)
        y (or (and (= dir (:down const/game-const)) 2)
              (and (= dir (:up const/game-const)) -2) 
              0)]
    {:x (+ (:x pos) x)
     :y (+ (:y pos) y)}))

(defn get-new-npos [due dir pos npos]
  (cond 
    (and (= (:y npos) 100) (>= (:x npos) 190) (= dir (:right const/game-const))) {:y 100 :x -10}
    (and (= (:y npos) 100) (<= (:x npos) -12) (= dir (:left const/game-const)))  {:y 100 :x 190}
    (= dir (:none const/game-const)) pos
    :else (if (not= due dir) (get-new-coord due pos) (get-new-coord dir pos))))

(defn facing-wall? [map pos npos dir]
  (and (on-grid-square? pos) (is-wall-space? map (next-pos npos dir))))

(defn get-new-direction [map due dir pos npos]
  (cond 
    (facing-wall? map pos npos dir) (:none const/game-const) 
    (and (not= due dir) (direction-allowable? map due dir pos npos)) due
    :else due))

(defn refresh-user-data [{user :user map :map :as state}] 
  (let [{due :due dir :direction
         pos :position npos :npos} user]
    (if (= (:phase state :playing))
      {:npos      (get-new-npos due dir pos npos)
        :position  npos
        :old-pos   pos
        :direction (get-new-direction map due dir pos npos)
        :due       dir}
      user)))

(defn pacman-move [state]
  (update-in state [:user]
    (fn [user]
      (merge user (refresh-user-data state)))))

(defn start-game [state]
  (-> state
    (assoc :state-changed false)))

(defn game-playing [state]
  (assoc state :phase :playing))

(defn game-dying [state]
  (if (> (- (:tick state) (:timer-start state)) (/ (const/FPS) 3))     
    (lose-life state)
    (-> state
      (redraw-block)
      (draw-dead (/ (:tick state) (* const/FPS 2))))))

(defn game-countdown [state]
  (let [diff (+ 1 (.floor js/Math (/ (- (:timer-start state) (:tick state)) const/FPS)))]
    (if (zero? diff)
      (-> state
        (assoc :phase :playing))
      (if-not (= diff (:last-time state))
        (-> state
          (assoc :last-time diff)
          (assoc :dialog (str "Starting in: " diff)))))))

(defn eaten-pill [state]
  (-> state
    (assoc :timer-start (:tick state))
    (assoc :eaten-count 0)))

;; =============================================================================
;; Game Loop

(defn driver [state]
  (let [phase (:phase state)
        state (-> (if-not (= phase :pause) 
                    (update-in state [:tick] (fnil inc 0))
                    state)
                (keydown)
                (update-in [:user] pacman-move))]
    (main-draw
      (cond 
        (= phase :playing) state
        (and (= phase :waiting) (:state-changed state)) (start-game state) 
        (and (= phase :eaten-pause) 
          (> (- (:tick state) (:timer-start state)) (* const/FPS 2))) (game-playing state)
        (= phase :dying) (game-dying state)
        (= phase :countdown) (game-countdown state)
        :else state))))

(defn make-state []
  (-> game-state
    (assoc :dialog "Press N to Start")
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
