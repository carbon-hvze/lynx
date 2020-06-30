(ns lynx.core
  (:require
   [lynx.expressions]
   [lynx.interpreter :as i]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defmacro f [kw args & body]
  `(do
     (swap! i/env assoc-in [::i/lambdas ~kw]
            {:name ~kw
             :args (quote ~args)
             :body (quote ~body)
             :f (fn ~args (do ~@body))})
     (get-in @i/env [::i/lambdas ~kw])))

(defn read-ns [ns-name]
  (if-let [namespc
           (-> (str/replace (name ns-name) "." "/")
               (str/replace  "-" "_")
               (#(str %1 ".lnx"))
               io/resource)]
    (read-string (slurp namespc))
    (throw (Exception. (str "namespace " ns-name " not found")))))

(defn prepare-expr [& [f & other]]
  (cond
    (list? f) (concat f other)
    (keyword? f) (concat (read-ns f) other)
    :else f))

(defn evaluate*
  ([expr] (i/eval-expr! (merge @i/env {::time 0}) (prepare-expr expr)))
  ([expr env & other]
   (let [expr* (apply prepare-expr expr other)]
     (i/eval-expr! (merge @i/env {::time 0} env) expr*))))

(defn evaluate [& args] (second (apply evaluate* args)))

(defn reset-env! [& args]
  (reset! i/env {}))

(comment
  (reset-env!)

  )
