(ns lynx.core-test
  (:require
   [matcho.core :as matcho]
   [lynx.core :as lynx]
   [lynx.log :as log]
   [clojure.test :refer :all]))

;; TODO add argcheck

;; add log to eval-exp!

;; TODO
(testing "natural language can be generated from lynx code")

;; always print current expr on exception
;; implement (verb (to noun)) form to be able to papametrize verbs with noun values
;; when lambda is failed to resolve throw meaningful exception (data error?)
;; add syntax validation tool
;; return data error instead of exception

#_(testing "restrictions can be applied"
    (def expr
      '(eval-list

        (to click button
            ;; nasty API call here
            (use identity))

        (to click button
            (makes button clicked))))

    (is (= (lynx/evaluate expr
                          {}
                          '(should button clicked)
                          '(do click button))

           '(should button clicked)))

    (is (lynx/evaluate expr
                       {}
                       '(do click button)
                       '(should button clicked))))

(deftest verbs

  (testing "verb are applied to nouns"
    (def expr
      '(to increment integer
           (use #(+ % 1))))

    (is (= 'increment (lynx/evaluate expr))))

  (testing "verb can be applied"
    (def expr
      '(eval-list

        (noun integer (positive number))

        (to increment integer
         (use #(+ % 1)))

        (increment integer)))

    (is (= 2 (lynx/evaluate expr {:integer 1}))))

  (testing "verb can be used with different nouns"
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

  (testing "verbs can have parameters"

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
            (use #(do (prn %2) (merge %1 {:sent true}))))

        ((set status) application rejected) 
        (mail application applicant-email)))

    (def env
      {:application
       ^{:about "My application to google ai"}
       {:text "Hey, we can work together"}})

    (matcho/assert
     {:sent true}
     (lynx/evaluate expr env))))

(deftest extending-with-code
  (testing "application logic is added with this interface"
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

  (testing "if you specify env as the first arg you get whole execution context"
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

(deftest control-flow
  (testing "use when to control flow"
    "body of when is not evaluated if conditions do not pass"

    (def state (atom 0))

    (def expr
      '(eval-list
        (to increment integer
            (use #(+ % 1)))

        (to set integer
            (use (fn [i] (reset! lynx.core-test/state 1))))

        (to (know-if integer odd)
            (use odd?))

        (to (know-if integer even)
            (use even?))

        (when (integer odd)
          (set integer)
          (increment integer))))

    (is (nil? (lynx/evaluate expr {:integer 2})))

    (is (= 0 @state))

    (is (= 4 (lynx/evaluate expr {:integer 3})))

    (is (= 1 @state))))
