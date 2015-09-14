(ns mill.ct)

;; Category theory niceties. Inspired by fluokitten,
;; but fluokitten isn't yet ported to cljs :(

(defprotocol Functor
  (fmap [x f]))

(defprotocol Applicative
  (pure [x y])
  (fapply [f a]))

(defprotocol Monad
  (bind [x f]))
