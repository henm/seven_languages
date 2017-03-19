;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns day2.core
  (:gen-class))

(defmacro unless
  "Implement an unless with an else condition using macros."
  [test ifBody elseBody]
  (list 'if (list 'not test) ifBody elseBody))

; Write a type using defrecord that implements a protocol.
; Since it is not specified what kind of protocol to implement, we will define
; a protocol with a single method and two implementations of this protocol. The
; method will take an argument and print different stuff based on the
; implementation.
; Not the most creative example...
(defprotocol ToyExample
  "A toy example for specifying a protocol."
  (print-message [this]))

(defrecord ToyExampleImpl1 [message] ToyExample
  (print-message [this]
    (println message " impl1!")))

(deftype ToyExampleImpl2 [message] ToyExample
  (print-message [this]
    (println message " impl2!")))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (unless true (println "Unless") (println "Not unless"))
  (unless false (println "Unless") (println "Not unless"))
  (def test1 (ToyExampleImpl1. "Hey"))
  (print-message test1)
  (def test2 (ToyExampleImpl2. "Hey"))
  (print-message test2)
)
