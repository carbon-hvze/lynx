(ns examples.midigen.core
  (:require
   [lynx.core :as lynx]
   [examples.midigen.scales :as scales]))

(def tail-expr
  '(then (list (chromatic sequence))
         (build scale gsharp locrian)))

;; TODO get rid of lnx extension and just use .edn files
(comment
  (lynx/evaluate :examples.midigen.definitions {} tail-expr))

