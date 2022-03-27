(ns midigen.scales
  (:require [lynx.core :as lynx]))

(def notes '[c csharp d dsharp e esharp f fsharp g gsharp a asharp b])

(def locrian-pattern [1, 2, 2, 1, 2, 2, 2])

(def distances (reduce (fn [acc v]
                         (if (empty? acc)
                           (conj acc v)
                           (conj acc (+ (last acc) v))))
                       [] locrian-pattern))

(lynx/f :scalegen [env _ root scale-type]
        (let [notes-seq (->> (:chromatic-sequence env)
                             (interleave (cycle notes))
                             (partition 2)
                             vec)
              in-scale (fn [[sym midi-idx]]
                         ;; TODO check for overflow
                         (->> distances
                              (map (fn [d] (+ d midi-idx)))
                              (map (fn [idx]
                                     (let [[note _] (get notes-seq idx)]
                                       (= note root))))
                              (some true?)))]

          (->> notes-seq
               (filter in-scale)
               (map first))))
