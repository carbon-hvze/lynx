(ns migigen.core
  (:require
   [lynx.core :as lynx]
   [midigen.scales :as scales]))

(def expr
  '(eval-list
    (noun (chromatic sequence) (all pitches from chromatic scale))

    (noun scale (system of pitches))

    (to list (chromatic sequence)
        (use (fn [_] (range 0 127))))

    (to build scale (use scalegen))

    (then (list (chromatic sequence))
          (build scale gsharp locrian))))

(lynx/evaluate expr {})
