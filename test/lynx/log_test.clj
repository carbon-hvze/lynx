(ns lynx.log-test
  (:require
   [matcho.core :as matcho]
   [lynx.core :as lynx]
   [lynx.log :as log]
   [clojure.test :refer :all]))

(defn log [expr]
  (-> expr
      (lynx/evaluate* {:letter-of-intent "Hello, my dear!"})
      first
      ::lynx/log))

(deftest execution-log

  (testing "execution is logged into the env"

    (def result
      (log '(eval-list
             (to send (letter of intent)
                 (use identity))
             (do send (letter of intent)))))

    (def log-example
      {:expression
       '(eval-list
         (to send (letter of intent) (use identity))
         (do send (letter of intent)))
       :result "Hello, my dear!"
       :subexp
       [{:expression '(to send (letter of intent) (use identity))
         :result 'send
         :subexp
         [{:expression 'send :result 'send}
          {:expression '(letter of intent) :quoted? true}
          {:expression '(use identity)
           :result string?
           :subexp [{:expression identity :result identity}]}]}
        {:expression '(do send (letter of intent))
         :result "Hello, my dear!"
         :subexp
         [{:expression 'send :result 'send}
          {:expression '(letter of intent) :quoted? true}]}]})

    (matcho/assert log-example result))

  (testing "search log for an expression by pattern"
    (def expr
      '(eval-list

        (to send (letter of intent)
            (use identity))

        (do send (letter of intent))))

    (def expected
      {:expression '(to send (letter of intent) (use identity))
       :result 'send})

    (matcho/assert (-> expr (log) (log/find '(to &))) expected)))
