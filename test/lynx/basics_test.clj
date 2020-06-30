(ns lynx.basics-test
  (:require
   [matcho.core :as matcho]
   [lynx.core :as lynx]
   [lynx.log :as log]
   [clojure.test :refer :all]))

(deftest composition-features

  (testing "there are clojure-like namespaces"
    (is (= (lynx/evaluate :examples.identity {:argument "420"}) "420")))

  (testing "lynx code can be grouped"
    (def expr
      '(eval-list
        (group (everything related to dogs)
               (noun dog (animal that barks)))

        (group (everything related to cats)
               (noun cat (animal that meows)))))

    (is (= '(animal that barks) (lynx/evaluate expr {} '(define dog))))
    (is (= '(animal that meows) (lynx/evaluate expr {} '(define cat)))))

  (testing "namespace can be read for ad-hoc processing"

    (matcho/assert
     '(eval-list
       (to return argument
           (use identity))
       (do return argument))
     (lynx/read-ns :examples.identity))))

(deftest computational-semantics
  (testing "something absurb evals to itself (i.e. has no meaning)"
    (def expr '(shmlona shmlena))

    (is (= (lynx/evaluate expr) expr)))

  (testing "all terms can be compound"
    (def expr
      '(eval-list
        (verb (send email) "contact w someone via email")

        (to (send email) candidate
            (use #(do (prn %) %)))

        (do (send email) candidate)))

    (def contact
      {:name "John McCarthy"
       :email "god-of-parens@gmail.com"})

    (matcho/assert
     contact
     (lynx/evaluate expr {:candidate contact})))

  (testing "compound terms are evaluated with convention"
    (def expr
      '(eval-list
        (to send (letter of intent)
            (use identity))
        (do send (letter of intent))))

    (is (= (lynx/evaluate expr {:letter-of-intent "Hello, my dear!"}) "Hello, my dear!"))))

(deftest execution-log

  (testing "execution is logged into the env"
    (def expr
      '(eval-list

        (to send (letter of intent)
            (use identity))

        (do send (letter of intent))))

    (def result
      (-> expr
          (lynx/evaluate* {:letter-of-intent "Hello, my dear!"})
          first
          ::lynx/log))

    (matcho/assert
     {:expression
      '(eval-list
        (to send (letter of intent)
            (use identity))
        (do send (letter of intent)))}
     result))

  (testing "some helpers are implemented for log"
    (def expr
      '(eval-list

        (to send (letter of intent)
            (use identity))

        (do send (letter of intent))))

    (def log (-> expr
                 (lynx/evaluate* {:letter-of-intent "Hello, my dear!"})
                 first
                 ::lynx/log))

    (matcho/assert
     {;;:start-time 0
      ;;:end-time 0
      :expression '(to send (letter of intent) (use identity))
      :result 'send}
     (log/find log '(to &)))))
