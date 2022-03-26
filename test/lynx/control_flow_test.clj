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

    (is (= 3 @state)))

  (testing "use then to pipe verbs"
    ;; TODO implement case two verbs - two nouns
    (def expr
      '(eval-list
        (to increment (my integer)
            (use #(+ % 1)))

        (to double (my integer)
            (use #(* % 2)))

        "verbs in then block update key bindings in context"

        (then
         (increment (my integer))
         (double (my integer)))))

    (is (= 8 (lynx/evaluate expr {:my-integer 3}))))

  (testing "use verb in then block to bind noun value"
    (def expr
      '(eval-list
        (to stream (ten ones)
            (use (fn [_] (take 10 (repeat 1)))))

        (to fold with (use fold))

        "verb can look in the env for noun binding"
        (then
         (stream (ten ones))
         (fold with +))))

    ;; TODO support destructuring in the first arg of lynx/f
    (lynx/f :fold [env _ op-symbol] (reduce (resolve op-symbol) (:ten-ones env)))

    (is (= 10 (lynx/evaluate expr {})))))



