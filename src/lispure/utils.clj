(ns lispure.utils)

(defn in? 
  "true if seq contains elem"
  [elem seq]
  (some #(= elem %) seq))

(defn third
  [seq]
  (second (rest seq)))