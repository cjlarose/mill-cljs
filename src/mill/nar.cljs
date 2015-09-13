(ns mill.nar
  (:require [mill.ct :refer [Functor Applicative fmap]]))

(def none {:byte-width 1
           :elements [{:valid? false
                       :buffer (js/Buffer. [0])}]})
(def nar {:byte-width 1
          :elements [{:valid? false
                      :buffer (js/Buffer. [-1])}]})

(deftype None []
  Object
  (toString [_]
    "None")
  Functor
  (fmap [_ _]
    (None.))
  Applicative
  (pure [_]
    (None.))
  (fapply [_ _]
    (None.)))

(deftype NotNone [v]
  Object
  (toString [_]
    (str "NotNone " v))
  Functor
  (fmap [_ f]
    (NotNone. (f v)))
  Applicative
  (pure [x]
    (NotNone. x))
  (fapply [_ a]
    (fmap a v)))

(deftype NaR []
  Object
  (toString [_]
    "NaR")
  Functor
  (fmap [_ _]
    (NaR.)))

(deftype NotNaR [v]
  Object
  (toString [_]
    (str "NotNaR " v))
  Functor
  (fmap [_ f]
    (NotNaR. (f v))))
