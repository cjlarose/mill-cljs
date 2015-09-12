(ns mill.nar
  (:require [mill.slice :refer [from-octet-seq]]))

(def none (with-meta (from-octet-seq 1 [0]) {:valid? false}))
(def nar (with-meta (from-octet-seq 1 [-1]) {:valid? false}))
