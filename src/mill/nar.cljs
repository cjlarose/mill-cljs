(ns mill.nar
  (:require [mill.ct :refer [Functor]]))

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
    (None.)))

(deftype NotNone [v]
  Object
  (toString [_]
    (str "NotNone " v))
  Functor
  (fmap [_ f]
    (NotNone. (f v))))
