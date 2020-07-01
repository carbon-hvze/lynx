(ns lynx.utils
  (:require
   [clojure.string :as str]))

(defn ->keyword [term]
  (cond 
    (seq? term) (keyword (str/join "-" term))
    :else (keyword term)))

(defn get-noun [env noun]
  (or (get env (->keyword noun))
      (get-in env [:lynx.interpreter/terms noun :value])))

(defn resolve-noun [env noun]
  (let [from-env (get env (->keyword noun))
        term-cfg (get-in env [:lynx.interpreter/terms noun])]
    (cond
      from-env {:type :noun :symbol noun :value from-env}
      (not-empty term-cfg) term-cfg)))
