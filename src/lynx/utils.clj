(ns lynx.utils
  (:require
   [clojure.string :as str]))

(defn ->keyword [term]
  (cond 
    (list? term) (keyword (str/join "-" term))
    :else (keyword term)))
