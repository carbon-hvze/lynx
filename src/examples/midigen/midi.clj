(ns examples.midigen.midi
  (:require [overtone.midi :as midi]))

;; start virtual midi device with mac audio setup utility

(defn test-device []
  (defonce abl (midi/midi-out "iac"))
  (midi/midi-note-on abl 39 100)
  (midi/midi-play abl [40 47 40] [80 50 110] [250 500 250]))



