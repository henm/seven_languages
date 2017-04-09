(ns day3.core
  (:gen-class))

; Use refs to create a vector of accounts in memory. Create debit and credit
; functions to change the balance of an account.

(defn credit
  "Add a given amount"
  [accounts account-index amount]
  (alter (accounts account-index) + amount))

(defn debit
  "Subtract a given amount"
  [accounts account-index amount]
  (credit accounts account-index (- amount)))

(defn exercise-one
  "Example run for the first exercise"
  []
  (let [account (ref 0)
      accounts [account]]
    (dosync (credit accounts 0 5)
      (credit accounts 0 2)
      (debit accounts 0 3))
    (println @account)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Sleeping barber problem
;
; - A barber shop takes customers.
; - Customers arrive at random intervals, from ten to thirty milliseconds.
; - The barber shop has three chairs in the waiting room.
; - The barber shop has one barber and one barber chair.
; - When the barber's chair is empty, a customer sits in the chair, wakes up
;   the barber, and gets a haircut.
; - If the chairs are occupied, all new customers will turn away.
; - Haircuts take twenty milliseconds.
; - After a customer receives a haircut, he gets up and leaves.
;
; Write a multithreaded program to determine how many haircuts a barber can
; give in ten seconds.

(defn is-cutting?
  "Determine if the barber is cutting at the moment"
  [barber]
  (get @barber :cutting))

(defn wait-or-go
  "Wait if there is an unoccupied chair or leave"
  [customer chairs]
  (if (< (count @chairs) 3)
    (swap! chairs conj customer)))

(defn cut-hair
  "Get a haircut"
  [barber customer chairs]
  (do
    (send barber (fn [x] { :customers-served (get x :customers-served), :cutting true }))
    (send barber (fn [x]
      (do
        (Thread/sleep 20)
        { :customers-served (+ (get x :customers-served) 1), :cutting false })))
    (if (> (count @chairs) 0)
      (let [next-customer (peek @chairs)]
        (do
          (swap! chairs pop)
          (cut-hair barber next-customer chairs))))))

(defn customer-arrives
  "A customer arrives."
  [customer chairs barber]
  (if (is-cutting? barber)
    (wait-or-go customer chairs)
    (cut-hair barber customer chairs)))

(defn create-barber
  "Create a new barber"
  []
  (agent { :customers-served 0, :cutting false}))

(defn create-chairs
  "Create three chairs"
  []
  (atom []))

(defn exercise-two
  "Second exercise"
  []
  (let [start-time (System/currentTimeMillis)
      barber (create-barber)
      chairs (create-chairs)]
    (while (< (System/currentTimeMillis) (+ start-time 10000))
      (let [customer (agent false)]
        (do
          (customer-arrives customer chairs barber)
          (Thread/sleep (+ (rand 20) 10)))))
    (println (get @barber :customers-served))
    (shutdown-agents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main
  "Seven languages in seven weeks, Clojure day 3"
  [& args]
  (exercise-one)
  (exercise-two))