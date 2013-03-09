(ns pacman.audio)

(def game-state (atom {:files []
                       :end-events []
                       :progress-events []
                       :playing []}))

(defn load [name path cb]
  (let [f ()])
)

(defn progress [event name callback])

(defn disable-sound [])

(defn ended [name])

(defn play [name])

(defn pause [] (println "hi"))

(defn resume [])