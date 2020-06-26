(ns lynx.interpreter-test
  (:require
   [lynx.interpreter :as i]
   [lynx.expressions]
   [clojure.test :refer :all]
   [matcho.core :as matcho]))

(deftest pattern-matching
  (testing "& wildcard matches any expr"
    (def expr
      '(eval-list
        (to increment integer
         (use #(+ % 1)))
        (do (increment integer))))

    (matcho/assert
     {:pattern '(eval-list &)}
     (i/implementation @i/env expr)))

  (testing "& wildcard means zero or more times"
    (def expr
      '(do increment integer))

    (matcho/assert
     {:pattern '(do * '* &)}
     (i/implementation @i/env expr)))

  (testing "* wildcard works"
    (def expr
      '(to increment integer
           (use #(+ % 1))))

    (matcho/assert
     {:pattern '(to * '* (use *))}
     (i/implementation @i/env expr)))

  (testing "weights work"
    (def expr
      '(to (know-if integer even)
           (use even?)))

    (matcho/assert
     {:pattern '(to (know-if '* *) (use *))}
     (i/implementation @i/env expr)))

  (testing "symbols are not matched"
    (is (nil? (i/implementation @i/env 'even?))))

  (testing "quoted & wildcard matched"
    (def expr
      '(when (is integer odd)
         (increment integer)))

    (matcho/assert
     {:pattern '(when * '&)}
     (i/implementation @i/env expr)))

  (testing "quoted subexpressions are matched"
    (def expr
      '(to (get client)
           (use identity)))

    (matcho/assert
     {:pattern '(to '(get *) (use *))}
     (i/implementation @i/env expr)))

  (testing "correct impl is matched"
    (matcho/assert
     {:pattern '(noun '* *)}
     (i/implementation @i/env '(noun a (some noun)))))

  (testing "symbols are matched to nothing"
    (is (nil? (i/implementation @i/env 'my-symbol)))))
