(ns lynx.verbs-test
  (:require
   [matcho.core :as matcho]
   [lynx.core :as lynx]
   [clojure.test :refer :all]))

(deftest verbs

  (testing "define verb to apply an action to a noun"
    (def expr
      '(to increment integer
           (use #(+ % 1))))

    (is (= 'increment (lynx/evaluate expr))))

  (testing "apply verb"
    (def expr
      '(eval-list

        (noun integer (positive number))

        (to increment integer
         (use #(+ % 1)))

        (increment integer)))

    (is (= 2 (lynx/evaluate expr {:integer 1}))))

  (testing "define verb for two nouns"
    (def expr
      '(eval-list

        (to (find first letter) integer
            (use (fn [n] (str (first (str n))))))

        (to (find first letter) string
            (use #(str (first %))))))

    (is (= (lynx/evaluate expr {:string "420" :integer 21}
                          '((find first letter) string))
           "4"))

    (is (= (lynx/evaluate expr
                          {:string "666" :integer 420}
                          '((find first letter) integer))

           "4")))

  (testing "add symbol parameter to a verb"

    (def expr
      '(eval-list

        (verb (set status) (sets document status))

        (to (set status) application
            (use (fn [application status]
                   (merge application {:status (str status)}))))

        (do (set status) application rejected)))

    (def env
      {:application
       ^{:about "My application to google ai"}
       {:text "Hey, we can work together"}})

    (matcho/assert
     (merge {:status "rejected"} (:application env))
     (lynx/evaluate expr env)))

  (testing "verbs can be applied sequentially"
    (def expr
      '(eval-list
        (verb (set status) (sets document status))

        (to (set status) application
            (use (fn [application status]
                   (merge application {:status (str status)}))))

        (to mail application
            (use #(do (prn %2) (prn %1) (merge %1 {:sent true}))))

        ((set status) application rejected) 
        (mail application applicant-email)))

    (def env
      {:application
       ^{:about "My application to google ai"}
       {:text "Hey, we can work together"}})

    (matcho/assert
     {:sent true}
     (lynx/evaluate expr env))))
