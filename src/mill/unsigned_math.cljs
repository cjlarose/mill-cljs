(ns mill.unsigned-math
  (:require [mill.buffer :refer [addu-buffers ->buffer]]
            [mill.nar :refer [result]]
            [mill.ct :refer [fapply fmap]]))

(def deadbeef '(222 173 190 239))

(defn- addu-buffer [x y]
  (let [[sum _] (addu-buffers x y)]
    (->buffer sum)))

(defn addu [x y]
  (fapply (fmap x #(partial addu-buffer %)) y))

(defn- addus-buffer [x y]
  (let [[sum carry] (addu-buffers x y)]
    (if (= carry 0)
      (->buffer sum)
      (->buffer (repeat (.-length x) 255)))))

(defn addus [x y]
  (fapply (fmap x #(partial addus-buffer %)) y))

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
