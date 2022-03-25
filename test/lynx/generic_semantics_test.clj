(ns lynx.generic-semantics-test
  (:require
   [matcho.core :as matcho]
   [lynx.core :as lynx]
   [clojure.test :refer :all]))

(deftest computational-semantics
  (testing "lynx is a lisp so it has symbols and lists"
    (is  (= (lynx/evaluate 'a) 'a))
    "note how something absurd just evaluates to itself"
    (is  (= (lynx/evaluate '(shlona shmlena)) '(shlona shmlena))))

  (testing "all compound terms are just lists wrapped in lists"
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

  (testing "some compound terms are special forms that are evaluated not like functions"
    (def expr
      '(eval-list
        (to send (letter of intent)
            (use identity))
        (do send (letter of intent))))

    (is (= (lynx/evaluate expr {:letter-of-intent "Hello, my dear!"}) "Hello, my dear!"))))

(deftest binding-features

  (testing "bind set-status function from context to a verb"
    (def expr
      '(eval-list

        (to (set status) application
            (use set-status))

        (do (set status) application accepted)))

    (lynx/f :set-status
            [app status]
            (merge app {:status (str status)}))

    (matcho/assert
     {:status "accepted"}
     (lynx/evaluate expr {:application {:name "Alan" :family "Kay"}})))

  (testing "set status receives whole execution context as a first arg"
    (def expr
      '(eval-list
        (to (set status) application
            (use set-status))

        (do (set status) application accepted)))

    (lynx/f
     :set-status
     [env app status]
     {:status (str status) :number (:number env)})

    (matcho/assert
     {:status "accepted" :number 1}
     (lynx/evaluate expr {:application {:name "Alan" :family "Kay"} :number 1}))

    (def expr
      '(eval-list
        (to scale number
            (use scale-number))

        (do scale number)))

    (lynx/f :scale-number [{:keys [factor] :as env} number]
            (* number factor))

    (def res
      (lynx/f :scale-number [{:keys [factor] :as env} number]
              (* number factor)))

    (= (lynx/evaluate expr {:number 5 :factor 5}) 25)))
