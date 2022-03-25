(ns lynx.control-flow-test
  (:require
   [matcho.core :as matcho]
   [lynx.core :as lynx]
   [clojure.test :refer :all]))

;; TODO add argcheck

;; add log to eval-exp!

(deftest control-flow
  (testing "use when to control flow"

    "expressions in when body are applied sequentially"

    (def state (atom 0))

    (def expr
      '(eval-list
        (to increment integer
            (use #(+ % 1)))

        (to cache integer
            (use (fn [i]
                  (reset! lynx.control-flow-test/state i))))

        (to (know-if integer odd)
            (use odd?))

        (when (integer odd)
          (cache integer)
          (increment integer))))

    (is (nil? (lynx/evaluate expr {:integer 2})))

    (is (= 0 @state))

    (is (= 4 (lynx/evaluate expr {:integer 3})))

    (is (= 3 @state))))


