(ns pacman.audio)

(def game-state (atom {:files []
                       :end-events []
                       :progress-events []
                       :playing []}))

(defn load [name path cb])

(defn progress [event name callback])

(defn disable-sound [])

(defn ended [name])

(defn play [name])

(defn pause [])

(defn resume [])