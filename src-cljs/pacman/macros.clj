(ns pacman.macros)

(defn set-expr [x props-and-exprs]
  `(set! (.. ~x ~@(butlast props-and-exprs)) ~(last props-and-exprs)))

(defmacro set-all! [x & exprs]
  `(do
     ~@(map #(set-expr x %) exprs)))

(comment
  (set-all! obj
    (-foo -bar -baz 5))
  )

