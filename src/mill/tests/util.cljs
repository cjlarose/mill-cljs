(ns mill.tests.util
  (:require [cljs.test :refer-macros [is]]))

(defn is-element [expected actual]
  (is (= (:valid? expected) (:valid? actual)))
  (is (.equals (js/Buffer. (clj->js (:buffer expected))) (:buffer actual))))

(defn is-element-nar [actual]
  (is (= false (:valid? actual)))
  (is (not= (reduce bit-or 0 (seq (:buffer actual))) 0)))
