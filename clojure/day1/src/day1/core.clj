;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns day1.core
  (:gen-class))

(defn big
  "Return true if a string is longer than n characters."
  [st n]
  (> (count st) n))

(defn collection-type
  "Return :list, :map or :vector based in the type of the collection col."
  [col]
  (cond
    (list? col) :list
    (map? col) :map
    (vector? col) :vector))

(defn -main
  "Day 1."
  [& args]
  (println (big "abc" 0)
           (big "abc" 4))
  (println (collection-type '(1 2 3))
           (collection-type [1 2 3])
           (collection-type { :one 1, :two 2}))
)
