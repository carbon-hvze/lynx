(ns lynx.log
  (:require [lynx.interpreter :as i]))

(defn find
  [{:keys [expression result subexp] :as log} pattern]
  (when expression
    (cond
      (first (i/match-expr pattern expression))
      (dissoc log :subexp)

      (not-empty subexp)
      (->> (map #(find % pattern) subexp)
           (filter identity)
           first))))
