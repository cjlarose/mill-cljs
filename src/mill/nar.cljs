(ns mill.nar
  (:require [mill.ct :refer [Functor Applicative fmap]]))

(def none {:byte-width 1
           :elements [{:valid? false
                       :buffer (js/Buffer. [0])}]})
(def nar {:byte-width 1
          :elements [{:valid? false
                      :buffer (js/Buffer. [-1])}]})

(deftype NaR []
  Object
  (toString [_]
    "NaR")
  Functor
  (fmap [_ _]
    (NaR.))
  Applicative
  (pure [_ _]
    (NaR.))
  (fapply [_ _]
    (NaR.)))

(deftype NotNaR [v]
  Object
  (toString [_]
    (str "NotNaR " v))
  Functor
  (fmap [_ f]
    (NotNaR. (f v)))
  Applicative
  (pure [_ y]
    (NotNaR. y))
  (fapply [_ a]
    (fmap a v)))
