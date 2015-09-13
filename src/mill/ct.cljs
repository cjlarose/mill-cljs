(ns mill.ct)

;; Category theory niceties. Inspired by fluokitten,
;; but fluokitten isn't yet ported to cljs :(

(defprotocol Functor
  "Something that can be mapped over"
  (fmap [x f]))
