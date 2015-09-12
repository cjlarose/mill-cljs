(ns mill.slice)

(defn slice
  "Makes a new, zeroed-out slice of the given byte-width and length"
  [length byte-width]
  {:byte-width byte-width
   :buffer (doto (js/Buffer. (* length byte-width))
             (.fill 0))})

(defn signed-scalar
  "Produces a scalar of width 4 for the given integer"
  [x]
  {:pre [(js/Number.isSafeInteger x)
         (< x (js/Math.pow 2 31))
         (>= x (- (js/Math.pow 2 31)))]}
  (let [buffer (doto (js/Buffer. 4) (.writeIntBE x 0 4))]
    {:byte-width 4 :buffer buffer}))

(defn from-octet-seq [byte-width octet-seq]
  {:byte-width byte-width
   :buffer (js/Buffer. (clj->js octet-seq))})

(defn octet-seq
  "Returns the list of octets of a slice"
  [{:keys [buffer]}]
  (map #(aget buffer %) (range (.-length buffer))))

(defn- elements
  "Returns a seq of buffers representing elements of the given slice"
  [{:keys [buffer byte-width]}]
  (map
    (fn [offset] (.slice buffer offset (+ offset byte-width)))
    (range 0 (.-length buffer) byte-width)))

(defn widen
  "Doubles the width of a slice, zero-extending each element to the left"
  [{:keys [buffer byte-width] :as s}]
  (let [old-length (.-length buffer)
        new-buffer (doto (js/Buffer. (* 2 old-length)) (.fill 0))]
    (doseq [[i view] (map-indexed vector (elements s))]
      (.copy view new-buffer (+ (* i 2 byte-width) byte-width)))
    {:byte-width (* 2 byte-width)
     :buffer new-buffer}))
