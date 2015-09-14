(ns mill.tests.util
  (:require [cljs.test :refer-macros [is]]))

(defn is-element [expected actual]
  (is (= (:valid? expected) (:valid? actual)))
  (is (= (:buffer expected) (seq (:value actual)))))

(defn is-element-nar [actual]
  (is (= false (:valid? actual))))
