(ns mill.core
  (:require [clojure.browser.repl :as repl]))

(def imaginary-member
  {:belt-size 8})

(defn machine [{:keys [belt-size] :as family-member}]
  {:belt (repeat belt-size :none)
   :family-member family-member})

(defn execute-op [{:keys [belt family-member] :as m} op & args]
  (->> (apply op belt args)
       (apply conj belt)
       (take (:belt-size family-member))
       (assoc m :belt)))

(defn none? [operand]
  (and (not (:valid? operand)) (= (:value operand) 0)))

(defn nar? [operand]
  (and (not (:valid? operand)) (not= (:value operand) 0)))

(defn w [constant]
  {:width 4 :scalarity 0 :valid? true :value constant})

(defn con [belt constant]
  [constant])

(defn belt-nth [belt pos-sym]
  (->> pos-sym
       (name)
       (drop 1)
       (apply str)
       (js/parseInt)
       (nth belt)))

(defn addf [belt a b]
  (let [lhs (belt-nth belt a)
        rhs (belt-nth belt b)]
    (cond
      (or (nar? lhs) (nar? rhs))
        [{:width 4 :scalarity 0 :valid? false :value -1}]
      (or (none? lhs) (none? rhs))
        [{:width 4 :scalarity 0 :valid? false :value 0}]
      :else
        [{:width 4 :scalarity 0 :valid? true :value (+ (:value lhs) (:value rhs))}])))

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(println "Hello world!")
