(ns lynx.core-test
  (:require
   [matcho.core :as matcho]
   [lynx.core :as engine]
   [clojure.test :refer :all]))

(deftest lynx-test
  (testing "something absurb evals to itself (i.e. has no meaning)"
    (def expr '(shmlona shmlena))

    (is (= (engine/evaluate expr) expr)))

  (testing "evaluates verb correctly"
    (def expr
      '(eval-list
        (to
         (increment integer)
         (use #(+ % 1)))
        (do (increment integer))))
    (is (= (engine/evaluate expr {:integer 1}) 2)))

  (testing "verb can be used with different nouns"
    (def expr
      '(eval-list
        (to
         (find-first-letter integer)
         (use (fn [n] (str (first (str n))))))
        (to
         (find-first-letter string)
         (use #(str (first %))))))

    (is (= (engine/evaluate expr {:string "420" :integer 21}
                            '(do (find-first-letter string)))
           "4"))

    (is (= (engine/evaluate expr {:string "666" :integer 420} 
                            '(do (find-first-letter integer)))
           "4")))

  (testing "noun can have adjective value"
    (def expr
      '(eval-list
        (to
         (know-if integer even)
         (use even?))
        (is integer even)))

    (is (true? (engine/evaluate expr {:integer 4})))
    (is (false? (engine/evaluate expr {:integer 5}))))

  (testing "use when to control flow"
    (def expr
      '(eval-list
        (to
         (increment integer)
         (use #(+ % 1)))
        (to
         (know-if integer odd)
         (use odd?))
        (when (and (is integer odd)
                   (is integer odd))
          (do (increment integer)))))

    (is (= (engine/evaluate expr {:integer 3}) 4))
    (is (nil? (engine/evaluate expr {:integer 4}))))

  (testing "nouns, verbs & adjectives can have definitions"
    (def expr
      '(eval-list
        (noun integer "just a positive number")
        (define integer)))

    (is (= (engine/evaluate expr) "just a positive number")))

  (testing "natural language can be generated from lynx code"
    (def expr
      '(expl-list
        (noun integer "just a positive number")

        (verb increment "add 1 to a number")

        (adj odd "does not divide by 2")

        (when (is integer odd)
          (do (increment integer)))))

    (is (= (engine/evaluate expr)
           "A integer is just a positive number. To increment means to add 1 to a number. If something is odd it means it does not divide by 2. If integer is odd we do increment a integer")))

  (testing "restrictions can be applied"
    (def expr
      '(eval-list
        (to (click button)
            ;; nasty API call here
            (use identity))
        (to (click button)
            (makes button clicked))))

    (is (= (engine/evaluate expr
                            {}
                            '(should button clicked)
                            '(do (click button)))
           '(should button clicked)))

    (is (engine/evaluate expr
                         {}
                         '(do (click button))
                         '(should button clicked))))

  (testing "all terms can be compound"
    (def expr
      '(eval-list
        (verb (send email) "contact w someone via email")

        (to ((send email) candidate)
            (use #(do (prn %) %)))

        (do ((send email) candidate))))

    (def contact
      {:name "John McCarthy"
       :email "god-of-parens@gmail.com"})

    (matcho/assert
     contact
     (engine/evaluate expr {:candidate contact})))

  (testing "verbs can have parameters"
    (def expr
      '(eval-list
        (verb (set status) "sets document status")

        (to ((set status) application)
            (use (fn [application status]
                   (merge application {:status (str status)}))))

        (do ((set status) application rejected))))

    (def env
      {:application
       ^{:about "My application to google ai"}
       {:text "Hey, we can work together"}})

    (matcho/assert
     (merge {:status "rejected"} (:application env))
     (engine/evaluate expr env)))

  (testing "application logic is added with this interface"
    (def expr
      '(eval-list
        (to ((set status) application)
            (use set-status))

        (do ((set status) application accepted))))

    (defmethod engine/f :set-status
      [_ app status]
      (merge app {:status (str status)}))

    (matcho/assert
     {:status "accepted"}
     (engine/evaluate expr {:application {:name "Alan" :family "Kay"}})))

  (testing "compound terms are evaluated with convention"
    (def expr
      '(eval-list
        (to (send (letter of intent))
            (use identity))
        (do (send (letter of intent)))))

    (is (= (engine/evaluate expr {:letter-of-intent "Hello, my dear!"}) "Hello, my dear!"))))

