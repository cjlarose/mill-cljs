(ns mill.core
  (:require [clojure.browser.repl :as repl]))

(def imaginary-member
  {:belt-size 8})

(defn machine [{:keys [belt-size] :as family-member}]
  {:belt (repeat belt-size :none)
   :family-member family-member})

(defn execute-op [{:keys [belt family-member] :as m} op & args]
  (->> (apply op belt args)
       (apply conj belt)
       (take (:belt-size family-member))
       (assoc m :belt)))

(defn con [belt constant]
  [constant])


;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(println "Hello world!")
