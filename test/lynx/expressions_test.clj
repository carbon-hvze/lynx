(ns lynx.expressions-test
  (:require
   [lynx.core :as lynx]
   [clojure.test :refer :all]
   [matcho.core :as matcho]))

(deftest logic-predicates
  (testing "and predicate"
    (def expr
      '(eval-list
        (to (know-if number odd)
            (use odd?))
        (to (know-if number positive)
            (use pos?))

        (when (and (number odd)
                   (number positive))
          okay)))

    (is (= 'okay (lynx/evaluate expr {:number 3})))
    (is (nil? (lynx/evaluate expr {:number 4}))))

  (testing "not predicate"
    (def expr
      '(eval-list
        (to (know-if number odd)
            (use odd?))
        (to (know-if number positive)
            (use pos?))

        (when (and (not (number odd))
                   (number positive))
          okay)))

    (is (nil? (lynx/evaluate expr {:number 3})))
    (is (= 'okay (lynx/evaluate expr {:number 4}))))

  (testing "no is used to check if noun value exists"
    (def expr
      '(when (no integer)
         (return this list)))

    (matcho/assert
     '(return this list)
     (lynx/evaluate expr {}))

    (is (nil? (lynx/evaluate expr {:integer 3})))))
