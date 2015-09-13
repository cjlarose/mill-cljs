(ns mill.nar
  (:require [mill.ct :refer [Functor Applicative fmap]]))

(def none {:byte-width 1
           :elements [{:valid? false
                       :buffer (js/Buffer. [0])}]})
(def nar {:byte-width 1
          :elements [{:valid? false
                      :buffer (js/Buffer. [-1])}]})

(deftype MaybeNaR [valid? value]
  Object
  (toString [_]
    (str (if valid? "Result" "NaR") " " value))
  Functor
  (fmap [this f]
    (if valid?
      (MaybeNaR. true (f value))
      this))
  Applicative
  (pure [_ y]
    (MaybeNaR. true y))
  (fapply [this a]
    (if valid?
      (fmap a value)
      this)))
