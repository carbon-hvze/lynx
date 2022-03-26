(ns examples.midigen
  (:require [overtone.midi :as midi]
            [lynx.core :as lynx]))

(defonce abl (midi/midi-out "iac"))

(midi/midi-note-on abl 39 100)

(midi/midi-play abl [40 47 40] [80 50 110] [250 500 250])

(def expr '(eval-list
            (noun (chromatic sequence) (all pitches from chromatic scale))

            (noun scale (system of pitches))

            (to list (chromatic sequence)
                (use (fn [_] (range 0 127))))

            (to build scale (use scalegen))

            (then (list (chromatic sequence))
                  (build scale gsharp locrian))))

(lynx/f :scalegen [env _ root scale-type]
        (reduce + (:chromatic-sequence env)))

(lynx/evaluate expr {})



