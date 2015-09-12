(ns mill.core
  (:require [mill.nar :refer [none]]))

(def tin
  {:belt-size 8
   :max-scalar-size 8
   :max-operand-size 8})

(defn machine [{:keys [belt-size] :as family-member}]
  {:belt (repeat belt-size none)
   :family-member family-member})

(defn execute-op [{:keys [belt family-member] :as m} op & args]
  (->> (apply op belt args)
       (apply conj belt)
       (take (:belt-size family-member))
       (assoc m :belt)))

; (defn none? [operand]
;   (and (not (:valid? operand)) (= (:value operand) 0)))
; 
; (defn nar? [operand]
;   (and (not (:valid? operand)) (not= (:value operand) 0)))

; (defn belt-nth [belt pos-sym]
;   (->> pos-sym
;        (name)
;        (drop 1)
;        (apply str)
;        (js/parseInt)
;        (nth belt)))

(enable-console-print!)
