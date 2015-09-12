(ns mill.core
  (:require [mill.slice :refer [octet-seq->slice]]))

(def tin
  {:belt-size 8
   :max-scalar-size 8
   :max-operand-size 8})

(def none {:valid? false
           :slice (octet-seq->slice 1 [0])})
(def nar {:valid? false
          :slice (octet-seq->slice 1 [-1])})

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

(defn belt-nth [belt pos-sym]
  (->> pos-sym
       (name)
       (drop 1)
       (apply str)
       (js/parseInt)
       (nth belt)))

(enable-console-print!)
