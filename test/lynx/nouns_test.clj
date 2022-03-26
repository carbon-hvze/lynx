(ns lynx.nouns-test
  (:require
   [matcho.core :as matcho]
   [lynx.interpreter :as i]
   [lynx.core :as lynx]
   [lynx.log :as log]
   [clojure.test :refer :all]))

(deftest nouns
  (testing "add symbolic definition to a noun"
    (def expr
      '(eval-list

        (noun integer (just a positive number))

        (define integer)))

    (lynx/evaluate* expr)

    (is (= (lynx/evaluate expr) '(just a positive number))))

  (testing "bind noun value in env"
    ;; in this case both compound and simple terms resolve the same way

    (is (= 4 (lynx/evaluate '(integer) {:integer 4})))

    (is (= 4 (lynx/evaluate 'integer {:integer 4}))))

  (testing "define adjective that describes noun"
    (def expr
      '(eval-list

        (to (know-if integer even)
            (use even?))

        (integer even)))

    (is (true? (lynx/evaluate expr {:integer 4})))
    (is (false? (lynx/evaluate expr {:integer 5}))))

  (testing "bind noun with a special verb form, then define adjective and check if it applies to a nount"
    (def expr
      '(eval-list

        (noun client (our beloved client))

        (to (get client)
            (use get-client-data))

        (to (know-if client registered)
            (use (fn [client-name]
                   {:registered false
                    :name client-name})))

        (is client registered)))

    (lynx/f
     :get-client-data
     [{{nm :name} :db-user :as env}]
     nm)

    (matcho/assert
     {:registered false
      :name "Alan Kay"}
     (lynx/evaluate expr {:db-user {:name "Alan Kay"}})))

  (testing "if noun is not defined it can not have adj value"
    (def expr
      '(eval-list
        (to (know-if integer even)
            (use even?))))

    (is (nil? (lynx/evaluate expr {} '(is integer even))))
    (is (nil? (lynx/evaluate expr {} '(integer even)))))

  (testing "bind noun value with a function from context, then add a verb to a noun"
    (def expr
      '(eval-list

        (to (get client)
            (use get-client-data))

        (to greet client
            (use (fn [client-name]
                   (str "Hello, " client-name "!"))))

        (greet client)))

    (lynx/f
     :get-client-data
     [{{nm :name} :db-user :as env}]
     nm)

    (is (= "Hello, Alan Kay!" (lynx/evaluate expr {:db-user {:name "Alan Kay"}}))))

  #_(testing "adjective values can be deduced"
    ;; for now only if complementary adj
    ;; was checked explicitly
      (def expr
        '(eval-list

          (to (know-if integer even)
              (use even?))

          (can-be integer even odd)

          (integer even)

          (integer odd)))

      (matcho/assert
       [{::i/terms
         {'integer
          {:props
           {'even {:is false}
            'odd {:is true :deduced true}}}}}
        true?]
       (lynx/evaluate* expr {:integer 3}))

      (matcho/assert
       [{::i/terms
         {'integer
          {:props
           {'even {:is true}
            'odd {:is false :deduced true}}}}}
        false]
       (lynx/evaluate* expr {:integer 4}))))
