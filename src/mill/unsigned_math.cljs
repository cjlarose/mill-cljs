(ns mill.unsigned-math
  (:require [mill.buffer :refer [addu-buffers ->buffer]]
            [mill.nar :refer [result nar]]))

(defn addu [x y]
  (let [[sum _] (addu-buffers x y)]
    (result (->buffer sum))))

(defn addus [x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (result (->buffer sum))
      (result (->buffer (repeat (.-length x) 255))))))

(defn adduw [x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (->buffer sum)
      (let [new-bytes (apply conj (cons carry sum) (repeat (dec (.-length x)) 0))]
        (->buffer new-bytes)))))

(defn addux [x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (result (->buffer sum))
      (nar (.-length x)))))
