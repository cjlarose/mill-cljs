(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'mill.core
   :output-to "out/mill.js"
   :output-dir "out"})
