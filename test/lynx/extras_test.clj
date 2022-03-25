(ns lynx.extras-test
  (:require
   [matcho.core :as matcho]
   [lynx.core :as lynx]
   [clojure.test :refer :all]))

(deftest lynx-namespaces
  (testing "eval file from classpath - resources/examples/identity.lnx"
    (is (= (lynx/evaluate :examples.identity {:argument "420"}) "420")))

  (testing "read code from a namespace as data structure"
    (matcho/assert
     '(eval-list
       (to return argument
           (use identity))
       (do return argument))
     (lynx/read-ns :examples.identity))))

(deftest visual-features
  (testing "code can be visually grouped"
    (def expr
      '(eval-list
        (group (everything related to dogs)
               (noun dog (animal that barks)))

        (group (everything related to cats)
               (noun cat (animal that meows)))))

    (is (= '(animal that barks) (lynx/evaluate expr {} '(define dog))))
    (is (= '(animal that meows) (lynx/evaluate expr {} '(define cat))))))

#_(testing "natural language can be generated from lynx code")

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

