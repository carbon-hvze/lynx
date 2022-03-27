(ns examples.midigen.core-test
  (:require
   [matcho.core :as matcho]
   [examples.midigen.core :as midigen]
   [lynx.core :as lynx]
   [clojure.test :refer :all]))

(deftest locrian-scale
  (testing "scale contains correct notes"

    (def expr
      '(then (list (chromatic sequence))
             (build scale gsharp locrian)))

    (def scale (lynx/evaluate :examples.midigen.definitions {} expr))

    (is (contains? (set (distinct scale)) 'gsharp))

    (is (= (count (distinct scale)) 7))))
