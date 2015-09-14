(ns mill.slice
  (:require [mill.nar :refer [result]]))

(defn scalar?
  "Returns whether the slice is a scalar. Scalars in Mill are just
  vectors of length 1"
  [{:keys [elements]}]
  (= (count elements) 1))

(defn- valid-int? [x]
  (and
    (js/Number.isSafeInteger x)
    (< x (js/Math.pow 2 31))
    (>= x (- (js/Math.pow 2 31)))))

(defn int-slice
  "Produces a vector of byte-width 4 for the given seq of integers"
  [& xs]
  {:pre [(every? valid-int? xs)]}
  {:byte-width 4
   :elements (map #(result (doto (js/Buffer. 4) (.writeInt32BE % 0))) xs)})

(defn split-slice
  [{:keys [byte-width elements]}]
  (let [halves (split-at (/ (count elements) 2) elements)]
    (mapv (fn [v] {:byte-width byte-width :elements v}) halves)))
