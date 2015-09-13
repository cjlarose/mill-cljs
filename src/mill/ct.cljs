(ns mill.ct)

;; Category theory niceties. Inspired by fluokitten,
;; but fluokitten isn't yet ported to cljs :(

(defprotocol Functor
  (fmap [x f]))

(defprotocol Applicative
  (pure [x])
  (fapply [f a]))
