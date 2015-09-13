(ns mill.tests.util
  (:require [cljs.test :refer-macros [is]]))

(defn is-element [expected actual]
  (is (= (:valid? expected) (:valid? actual)))
  (is (.equals (js/Buffer. (clj->js (:buffer expected))) (:buffer actual))))
