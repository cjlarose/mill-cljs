(ns mill.unsigned-math
  (:require [mill.buffer :refer [addu-buffers ->buffer]]
            [mill.nar :refer [result nar]]))

(defn- add [if-overflow x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (result (->buffer sum))
      (if-overflow sum carry))))

(def addu (partial add (fn [sum _] (result (->buffer sum)))))
(def addus (partial add (fn [sum _] (result (->buffer (repeat (count sum) 255))))))
(def addux (partial add (fn [sum _] (nar (count sum)))))

(defn adduw [x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (->buffer sum)
      (let [new-bytes (apply conj (cons carry sum) (repeat (dec (.-length x)) 0))]
        (->buffer new-bytes)))))
