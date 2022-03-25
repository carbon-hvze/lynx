(ns examples.midi-gen
  (:require [overtone.midi :as midi]
            [lynx.core :as lynx]))

(defonce abl (midi/midi-out "iac"))

(midi/midi-note-on abl 39 100)

(midi/midi-play abl [40 47 40] [80 50 110] [250 500 250])

(def expr "(hello world)")

(lynx/evaluate expr {})



