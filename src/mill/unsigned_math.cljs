(ns mill.unsigned-math
  (:require [mill.buffer :refer [addu-buffers ->buffer]]))

(def deadbeef '(222 173 190 239))

(defn addu [x y]
  (let [[sum _] (addu-buffers x y)]
    (->buffer sum)))

(defn addus [x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (->buffer sum)
      (->buffer (repeat (.-length x) 255)))))

(defn adduw [x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (->buffer sum)
      (let [new-bytes (apply conj (cons carry sum) (repeat (dec (.-length x)) 0))]
        (->buffer new-bytes)))))

(defn addux [x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (->buffer sum)
      (->buffer (take (.-length x) (cycle deadbeef))))))
