(ns mill.nar
  (:require [mill.ct :refer [Functor Applicative fmap Monad]]))

(defrecord MaybeNaR [valid? value]
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
      this))
  Monad
  (bind [this f]
    (if valid?
      (f value)
      this)))

(defn result [v]
  (MaybeNaR. true v))

(def deadbeef '(222 173 190 239))

(defn nar [width]
  (MaybeNaR. false (js/Buffer. (clj->js (take width (cycle deadbeef))))))
